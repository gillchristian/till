use crate::capabilities::{Clock, Exec, Output, Sleeper};
use std::time::Duration;

pub struct Engine<E, S, C> {
    exec: E,
    sleep: S,
    clock: C,
}

impl<E, S, C> Engine<E, S, C> {
    pub fn new(exec: E, sleep: S, clock: C) -> Self {
        Self { exec, sleep, clock }
    }
}

impl<E, S, C> Engine<E, S, C>
where
    E: Exec,
    S: Sleeper,
    C: Clock,
{
    /// Run the command once and return its output.
    pub async fn run_once(&self, cmd: &str) -> anyhow::Result<Output> {
        self.exec.run(cmd).await
    }

    /// Sleep using the injected sleeper.
    pub async fn nap(&self, d: Duration) {
        self.sleep.sleep(d).await;
    }

    /// Current time via injected clock (good for tests).
    pub fn now(&self) -> chrono::DateTime<chrono::Local> {
        self.clock.now()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capabilities::{Clock, Exec, Output, Sleeper};
    use async_trait::async_trait;
    use chrono::{FixedOffset, TimeZone};
    use std::{sync::{Arc, Mutex}, time::Duration};

    struct FakeExec {
        results: Mutex<Vec<anyhow::Result<Output>>>,
    }

    impl FakeExec {
        fn new(results: Vec<anyhow::Result<Output>>) -> Self {
            Self { results: Mutex::new(results) }
        }
    }

    #[async_trait]
    impl Exec for FakeExec {
        async fn run(&self, _cmd: &str) -> anyhow::Result<Output> {
            let mut lock = self.results.lock().unwrap();
            if lock.is_empty() {
                anyhow::bail!("no more results")
            } else {
                lock.remove(0)
            }
        }
    }

    struct FakeSleeper {
        pub calls: Arc<Mutex<Vec<Duration>>>,
    }

    #[async_trait]
    impl Sleeper for FakeSleeper {
        async fn sleep(&self, d: Duration) {
            self.calls.lock().unwrap().push(d);
        }
    }

    #[derive(Clone, Copy)]
    struct FakeClock {
        now_val: chrono::DateTime<chrono::Local>,
    }

    impl Clock for FakeClock {
        fn now(&self) -> chrono::DateTime<chrono::Local> {
            self.now_val
        }
    }

    fn fixed_local(y: i32, m: u32, d: u32, hh: u32, mm: u32, ss: u32) -> chrono::DateTime<chrono::Local> {
        let fixed = FixedOffset::east_opt(0).unwrap().with_ymd_and_hms(y, m, d, hh, mm, ss).unwrap();
        fixed.with_timezone(&chrono::Local)
    }

    #[tokio::test]
    async fn run_once_returns_first_result() {
        let exec = FakeExec::new(vec![
            Ok(Output { status: 0, stdout: "a".into(), stderr: "".into() }),
            Ok(Output { status: 1, stdout: "b".into(), stderr: "err".into() }),
        ]);
        let sleeper = FakeSleeper { calls: Arc::new(Mutex::new(vec![])) };
        let clock = FakeClock { now_val: fixed_local(2025, 1, 1, 0, 0, 0) };

        let engine = Engine::new(exec, sleeper, clock);

        let out = engine.run_once("echo hi").await.unwrap();
        assert_eq!(out.status, 0);
        assert_eq!(out.stdout, "a");
        assert!(out.stderr.is_empty());
    }

    #[tokio::test]
    async fn nap_records_durations_via_sleeper() {
        let exec = FakeExec::new(vec![]);
        let calls = Arc::new(Mutex::new(vec![]));
        let sleeper = FakeSleeper { calls: calls.clone() };
        let clock = FakeClock { now_val: fixed_local(2025, 1, 1, 0, 0, 0) };
        let engine = Engine::new(exec, sleeper, clock);

        engine.nap(Duration::from_millis(5)).await;
        engine.nap(Duration::from_secs(2)).await;

        let got = calls.lock().unwrap().clone();
        assert_eq!(got, vec![Duration::from_millis(5), Duration::from_secs(2)]);
    }

    #[test]
    fn now_uses_injected_clock() {
        let exec = FakeExec::new(vec![]);
        let sleeper = FakeSleeper { calls: Arc::new(Mutex::new(vec![])) };
        let expected = fixed_local(2030, 12, 31, 23, 59, 58);
        let clock = FakeClock { now_val: expected };
        let engine = Engine::new(exec, sleeper, clock);

        let got = engine.now();
        assert_eq!(got, expected);
    }
}

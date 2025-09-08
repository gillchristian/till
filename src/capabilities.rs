use async_trait::async_trait;
use chrono::{DateTime, Local};
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct Output {
    pub status: i32,
    pub stdout: String,
    pub stderr: String,
}

#[async_trait]
pub trait Exec: Send + Sync {
    async fn run(&self, cmd: &str) -> anyhow::Result<Output>;
}

#[async_trait]
pub trait Sleeper: Send + Sync {
    async fn sleep(&self, d: Duration);
}

pub trait Clock: Send + Sync {
    fn now(&self) -> DateTime<Local>;
}

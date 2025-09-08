use clap::Parser;
use std::time::Duration;

use till::engine::Engine;
use till::interpreters::cli::{CliExec, SystemClock, TokioSleeper};
use till::matchers::{parse_matcher, run_matchers, Matcher};
use till::notifications::{notify_error, notify_match_found};
use till::ui;

#[derive(Parser)]
#[command(
    name = "till",
    version = "0.0.1",
    about = "Execute a command until its output matches certain conditions.",
    long_about = "till v0.0.1\nExecute a command until its output matches certain conditions.\nProject's Home Page: https://github.com/gilchristian/till"
)]
struct Args {
    /// Command to run
    cmd: String,

    /// Patterns (mini expr: quotes allowed; ! negates; & and | combine)
    patterns: Vec<String>,

    /// Interval (seconds) to run CMD
    #[arg(long, default_value = "2")]
    interval: u64,

    /// Keep trying if CMD exits with non-zero status
    #[arg(long)]
    continue_on_error: bool,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Parse patterns -> matchers
    let mut matchers: Vec<Matcher> = Vec::new();
    for p in &args.patterns {
        match parse_matcher(p) {
            Ok(m) => matchers.push(m),
            Err(e) => {
                eprintln!("Error parsing pattern '{p}': {e}");
                std::process::exit(1);
            }
        }
    }
    if matchers.is_empty() {
        eprintln!("No valid patterns provided");
        std::process::exit(1);
    }

    // Wire engine with CLI interpreters
    let engine = Engine::new(CliExec, TokioSleeper, SystemClock);
    let header = format!("$ {}", &args.cmd);

    loop {
        let out = engine.run_once(&args.cmd).await?;

        // Error policy (domain is neutral; CLI decides)
        if !args.continue_on_error && out.status != 0 {
            if !out.stderr.trim().is_empty() {
                eprintln!("{}", out.stderr);
            }
            notify_error(&args.cmd, out.status);
            std::process::exit(out.status);
        }

        // Render
        let lines: Vec<String> = out.stdout.lines().map(|s| s.to_string()).collect();
        ui::output_header_and_content(engine.now(), &header, &lines)?;

        // Check assertions
        if run_matchers(&matchers, &lines) {
            println!();
            println!("Found match, exiting ...");
            notify_match_found(&args.cmd);
            break;
        }

        // Wait and retry
        engine.nap(Duration::from_secs(args.interval)).await;
    }

    Ok(())
}

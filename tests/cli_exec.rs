use till::capabilities::Exec;
use till::interpreters::cli::CliExec;

#[tokio::test]
async fn cli_exec_runs_echo_command() {
    let exec = CliExec;
    let out = exec.run("echo hello-from-cli-exec").await.expect("run failed");
    assert_eq!(out.status, 0);
    assert!(out.stdout.contains("hello-from-cli-exec"));
    // stderr should typically be empty
    assert!(out.stderr.is_empty());
}

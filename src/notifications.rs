use notify_rust::{Notification, Timeout};

/// Send a desktop notification when till finishes successfully
pub fn notify_success(cmd: &str) {
    let _ = Notification::new()
        .summary("Till - Command Completed")
        .body(&format!("Command '{}' finished successfully", cmd))
        .icon("terminal")
        .timeout(Timeout::Milliseconds(5000))
        .show();
}

/// Send a desktop notification when till finishes with an error
pub fn notify_error(cmd: &str, status: i32) {
    let _ = Notification::new()
        .summary("Till - Command Failed")
        .body(&format!("Command '{}' failed with exit status {}", cmd, status))
        .icon("error")
        .timeout(Timeout::Milliseconds(8000))
        .show();
}

/// Send a desktop notification when till finds a match and exits
pub fn notify_match_found(cmd: &str) {
    let _ = Notification::new()
        .summary("Till - Match Found")
        .body(&format!("Pattern matched for command '{}'", cmd))
        .icon("dialog-information")
        .timeout(Timeout::Milliseconds(5000))
        .show();
}

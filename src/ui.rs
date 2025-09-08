use chrono::{DateTime, Local};
use crossterm::{
    cursor, execute,
    terminal::{self, ClearType},
};
use std::io::{self, Write};

fn format_time(dt: DateTime<Local>) -> String {
    format!("{}", dt.format("%Y/%m/%d %H:%M:%S"))
}

fn make_header(header: &str, width: usize, date: &str) -> String {
    let n = width.saturating_sub(3 + header.len() + date.len());
    let space = " ".repeat(n);
    format!("{}{}[{}] ", header, space, date)
}

fn reset_screen() -> Result<(), io::Error> {
    execute!(
        io::stdout(),
        terminal::Clear(ClearType::All),
        cursor::MoveTo(0, 0)
    )?;
    Ok(())
}

pub fn output_header_and_content(
    now: DateTime<Local>,
    header: &str,
    content: &[String],
) -> Result<(), io::Error> {
    let (width, height) = terminal::size()?;

    reset_screen()?;

    println!("{}", make_header(header, width as usize, &format_time(now)));
    println!();

    // Fit content in terminal height
    let max_lines = (height as usize).saturating_sub(2);
    for line in content.iter().take(max_lines) {
        println!("{line}");
    }

    io::stdout().flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_line_fits_and_ends_with_date() {
        let header = "$ echo hi";
        let date = "2025/01/01 00:00:00";
        let width = 40;

        let line = make_header(header, width, date);

        // must start with header
        assert!(line.starts_with(header));

        // must contain bracketed date at the end (followed by a space)
        let needle = format!("[{}] ", date);
        assert!(line.ends_with(&needle));

        // Expected spaces = width - (3 + header.len() + date.len()), saturating
        let expected_spaces = width.saturating_sub(3 + header.len() + date.len());
        let actual_spaces = line.len() - header.len() - needle.len();
        assert_eq!(actual_spaces, expected_spaces);
    }
}

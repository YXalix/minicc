

pub fn extract_number_and_endpoint(input: &str) -> Option<(i32, usize)> {
    // 查找第一个非数字字符的索引
    let mut now_index = 0;
    for c in input.chars() {
        if !c.is_digit(10) {
            break;
        }
        now_index += 1;
    }
    if now_index == 0 {
        return None;
    } else {
        let number = input[..now_index].parse::<i32>().unwrap();
        return Some((number, now_index));
    }
}

pub fn read_punct(input: &str) -> Option<usize> {
    if input.len() == 0 {
        return None;
    }
    if input.starts_with("==") || input.starts_with("!=") || input.starts_with("<=") || input.starts_with(">=") {
        return Some(2);
    }
    let c = input.chars().nth(0).unwrap();
    return if c.is_ascii_punctuation() {
        Some(1)
    } else {
        None
    };
}

// 对齐到Align的整数倍
pub fn align_to(n: i32, align: i32) -> i32 {
    return (n + align - 1) / align * align;
}
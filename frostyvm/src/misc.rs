pub fn modify_in_place<T, F: Fn(T) -> T>(mut v: Vec<T>, f: F) -> Vec<T> {
    let n = v.len();
    if n > 0 {
        let mut t = v.pop().unwrap();
        for i in 0 .. n - 1 {
            let x = std::mem::replace(&mut v[i], t);
            let y = f(x);
            t = std::mem::replace(&mut v[i], y);
        }
        v.push(f(t));
    }
    v
}

#[test]
fn test_modify_inplace() {
  assert_eq!(modify_in_place(vec![1,2,3], |x| x * x), vec![1, 4, 9]);
}
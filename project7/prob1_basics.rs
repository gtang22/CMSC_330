// prob1_basics.rs 

// To get the tests up and running fast, you may wish to add all
// functions with empty bodies / dummy returns intitially
#![allow(unused_imports)]
#![allow(dead_code)]
use std::collections::HashSet;

/// Returns the sum 1 + 2 + ... + n
/// 
/// If n is less than 0, return -1
/// 
/// May use either a fixed equation to calculate the answer or an
/// iterative approach (though Gauss himself would likely prefer the
/// former...)
///
/// EXAMPLES:
/// gauss(5)  -> 15
/// gauss(10) -> 55
/// gauss(-2) -> -1
pub fn gauss(n: i32) -> i32 {
  if n < 0 {
    return -1;
  } 
  
  let top = n * (n + 1);
  return top/2;
}

  
/// Returns a count of elements in `slice` of i32that satisfy:
///   lo <= x <= hi
/// Uses an iterator over the slice to ensure all elements are visited.
///
/// EXAMPLES:
/// in_range([5,2,1,3,9], 2, 5)  -> 3
/// in_range([5,2,1,3,9], 3, 4)  -> 1
/// in_range([5,2,1,3,9], 2, 10) -> 4
/// in_range([], 2, 10)          -> 0
pub fn in_range(slice: &[i32], lo: i32, hi: i32) -> i32 {
  let mut count = 0;
  for num in slice {
    if lo <= *num && *num <= hi {
      count = count + 1;
    }
  }
  return count;
}

/// Calculates and returns the mean of elements in `slice` of floating
/// point values. To handle empty slices, this function returns an
/// Option: None for empty slices, and Some(mean) for filled slices.
///
/// EXAMPLES:
/// mean([])                     -> None
/// mean([10.0, 5.0, 7.0, 20.0]) -> Some(10.5)
/// mean([-10.0, 3.0, 2.0])      -> Some(-1.6666)
pub fn mean(slice: &[f64]) -> Option<f64> {
  if slice == [] {
    return None
  }
  let mut total = 0.0;
  let mut count = 0.0;
  for num in slice {
    total = total + *num;
    count = count + 1.0;
  }

  return Some(total/count);
}

/// Returns true if `slicea` is a subset of `sliceb` and false
/// otherwise. The function is generic so works with slices of any
/// type. The slices are not ordered so must be searched sequentially.
/// 
/// EXAMPLES:
///   subset([1,3,2], [1,2,3,4,5]) -> true
///   subset([1,3,2], [1,3,4,5])   -> false
///   subset(["a","c","d","c"], ["d","c","a"])     -> true
///   subset(["a","c","d","c"], ["d","c","a","r"]) -> true
///   subset(["a","q","d"],     ["d","c","a","r"]) -> false
///
/// NOTE: Utilizing certain methods of slices may lead to shorter code
/// by letting methods detect whether elements are contained or not.
fn is_in<T>(slice: &[T], element: &T) -> bool
  where T: PartialEq<T>       {

  for ele in slice {
    if *ele == *element {
      return true
    }
  }

  return false;
}

pub fn subset<T>(slicea: &[T], sliceb: &[T]) -> bool
  where T: PartialEq<T>       {
    let mut ans = true;

    for element in slicea {
      if is_in(sliceb, element) == false {
        ans = false;
        break;
      }
    }

    return ans;
  }  // to compare elements via ==, type must implement PartialEq Trait

/// Return a string showing the binary digits of the given unsigned
/// (positive) integer. Calculates the binary digits through repeated
/// division by 2 where the remainders become the digits in the binary
/// number. A survey of this method is here
/// https://www.cuemath.com/numbers/decimal-to-binary/
///
/// While converting, pushes digits of "0" or "1" into a container
/// type, vector being a good choice.  These digits need to be
/// reversed at the end of the computation (last digit found is most
/// significant, leftmost bit) so visits the digits in reverse order
/// in the container appending them to a String which is ultimately
/// returned.
///
/// No leading zeros are provided so the left-most character is always
/// a 1 EXCEPT in the special case of decimal value 0 which should
/// return the string "0".
/// 
/// EXAMPLES:
/// to_binstring(  0) ->         "0"
/// to_binstring(  2) ->        "10"
/// to_binstring(  9) ->      "1001"
/// to_binstring( 32) ->    "100000"
/// to_binstring(510) -> "111111110"
pub fn to_binstring(num: u32) -> String {
  if num == 0 {
    return String::from("0")
  }
  let mut new_num = num;
  let mut bit_string = String::from("");

  while new_num > 0 {
    if new_num & 1 == 0 {
      bit_string.insert_str(0, "0")
    }
    else {
      bit_string.insert_str(0, "1")
    }
    new_num = new_num >> 1;
  }

  return bit_string;
}

/// Construct a circulant matrix (2D vector) from the given
/// slice. Briefly a circulant matrix has row i rotated left by i
/// elements with row 0 being identical to the parameter r0_slice. A
/// vector or rows is allocated and each row is constructd as a
/// rotation left by i indices from the 0th row. The function is
/// generic and accepts any type that can be Cloned elements from
/// r0_slice must have clone() invoked on them to get a memory
/// distinct copy of them.
///
/// EXAMPLES:
/// 
/// circulant(&[1,2,3]) ->
///  [[1,2,3],
///   [2,3,1],
///   [3,1,2]]
///
/// circulant(&["a","b","c","d"]) ->
///  [["a","b","c","d"]
///   ["b","c","d","a"]
///   ["c","d","a","b"]
///   ["d","a","b","c"]]
fn shift_one<T>(slice: &[T]) -> Vec<T> 
  where T:Clone     {
    let mut vec = slice.to_vec();
    let temp = vec.remove(0);

    vec.push(temp);

    return vec;
}
pub fn circulant<T>(r0_slice: &[T]) -> Vec<Vec<T>>
  where T:Clone    {
    let mut temp_vec = r0_slice.to_vec();
    let mut ans_vec = vec![];

    ans_vec.push(temp_vec.to_vec());

    for _n in 0..r0_slice.len()-1 {
      temp_vec = shift_one(&temp_vec);
      ans_vec.push(temp_vec.to_vec())
    }

    return ans_vec;
}             // elements implement Clone so have .clone()


/// Returns a count of the number of "words" in `text`. The notion of
/// a word is a series of non-whitespace characters, (whitespace is
/// space, tab, newline). Regular expressions may be used for counting
/// words or character-by-character iteration to count whitespace
/// transitions. In either case, iteration through the string will be
/// required, either by regex match by direct iteration on the
/// characters.
/// 
/// NOTE: The Regular expression "crate" is marked as a dependency for
/// this project so will be downloaded HOWEVER you will need to add an
/// appropriate "use" statement to access its functionality here.
/// 
/// EXAMPLES:
/// count_words( &String::from("hello world"))                          -> 2
/// count_words( &String::from("       "))                              -> 0
/// count_words( &String::from("ALL ... NON - whitespace !! "))         -> 6
/// count_words( &String::from("tabs\tor spaces\tor\ttabs\tor spaces")) -> 7
pub fn count_words(text: &String) -> i32 {
  let mut count = 0;
  let mut in_word = false;

  for c in text.chars() {
    if in_word {
      if c == '\t' || c == ' ' || c == '\n' {
        in_word = false;
      }
    }
    else {
      if c != '\t' && c != ' ' && c != '\n' {
        in_word = true;
        count = count + 1;
      }
    }
  }

  return count;
}

use regex::Regex;
use std::io::BufReader;
use std::fs::File;
use std::io::BufRead;
use std::path::Path;
use edit_distance::edit_distance;

// NOTE: Add some use statements here to bring in functionality needed
// for file I/O, regular expressions, edit_distance.

// TODO: Complete the functions outlined below

/// Allocate a vector and read all newline-separated strings from the
/// given `fname` into it returning it. As the strings are read in,
/// convert them to all upper case for ease of later use. This
/// function may panic!() if there are problems with the file. Words
/// should appera in the vector in the same order that they appeared
/// in the file.
///
/// EXAMPLES:
/// load_stringvec("test-data/small-dict.txt") -> ["APPLE","BANANA","CARROT"]
/// load_stringvec("test-data/google-10000-english.txt") -> ["THE", "OF", "AND", "TO", ...]
pub fn load_string_upper(fname: &str) -> Vec<String> {
  let mut result = vec![];

  if Path::new(fname).exists() {
      let file = File::open(&fname).expect(&format!("Couldn't open file {fname}"));
      let reader = BufReader::new(file);

      for line in reader.lines() {
          let new_test = line.unwrap().to_uppercase();
          result.push(new_test);
      }

  } else {
      println!("Couldn't open file {}: No such file or directory (os error 2)", fname);
  }
  return result;
}

/// Iterate through the words in String `text` and construct a
/// corrected version of the string. Any word not contained in `dict`
/// is "marked" in the corrected version with double asterisks around
/// it.
/// 
/// WORDS / REGEXS: Words are defined as contiguous sequences of a-z
/// or A-Z or ' (single quote).  Using a regular expression to iterate
/// over words is likely helpful.  Portions of the original string in
/// between the corrected data should be copied into the corrected
/// version verbatim. Determining the starting / ending index of
/// matches is helpful for this.
///
/// CHECKING DICTIONARY: Words in `dict` are expected to be all upper
/// case so to check for the presence of a word in `dict`, it must
/// also be conveted to upper case, likely using a string method. Use
/// UPPERCASE versions of marked, incorrect words to make them easier
/// to see.
/// 
/// EXAMPLES:
/// let dict = vec!["APPLE","BANANA","ONION"];              // NOTE: types are slightly wrong, Sting vs str
/// mark_corrected("grape     apple  \n onion\n",&dict)     // string to correct
///             -> "**GRAPE**     apple  \n onion\n"        // corrected version
/// 
/// let dict = vec!["apple","banana","onion"];              
/// mark_corrected(" 12  3456 . ,,  78 0.123",&dict)        // string to correct
///             -> " 12  3456 . ,,  78 0.123"               // corrected version
/// 
/// let dict = vec!["ALL","BASE","ARE","YOUR","US"];        
/// mark_corrected("All your bass are belong 2 us!!",&dict) // string to correct
///             -> "All your **BASS* are **BELONG** 2 us!!" // corrected version
/// 
pub fn mark_corrected(text: &String, dict: &Vec<String>) -> String {
  let re = Regex::new(r"[a-zA-Z\']+").unwrap();
    let mut ans = String::from("");
    let mut index = 0;
    
    for element in re.captures_iter(text) {
        //dbg!(&element);
        match element.get(0) {
            Some(d) => {
                let gap_string = &text[index..d.start()];
                let captial_element = d.as_str().to_uppercase();
                let index_pos = dict.iter().position(|r| *r == captial_element);
                
                ans.push_str(gap_string);
                match index_pos {
                    Some(_i) => {
                        ans.push_str(d.as_str());
                    },
                    _ => {
                        ans.push_str("**");
                        ans.push_str(&captial_element);
                        ans.push_str("**");
                    }
                }
                index = d.end();
            },
            _ => {
                
            }
        }
    }
    
    ans.push_str(&text[index..text.len()]);
    ans
}


/// Sets up a placeholde for implementing several correction schemes
pub trait Corrector {
  /// Produce a corrected version of the given word, possibly marked
  /// as needing attention or having a correction provided
  fn correct_word(&mut self, word: &str) -> String;
}
  

/// Similar to `mark_corrected()` but uses a Corrector to produce the
/// corrected strings. Any word found while scanning `text` that is
/// does not have an upcased version in `dict` is passed to
/// `corrector.correct_word()` which will give produce a String to
/// replace it with in the corrected version. The general algorithm
/// and specific considerations are identical to `mark_corrected()`
/// 
/// EXAMPLES:
/// let dict = vec!["APPLE","BANANA","ONION"];              
/// let mut mc = MarkCorrector::new(">>","<<");             // use marking corrector
/// mark_corrected("grape     apple  \n onion\n",&dict,mc)  // string to correct
///             -> ">>GRAPE<<     apple  \n onion\n"        // corrected version
/// 
/// let dict = vec!["ALL","BASE","ARE","YOUR","US"];          
/// let mut ac = AutoCorrector::new(&dict,false);             // use auto corrector
/// mark_corrected("All your bass are belong 2 us!!",&dict,ac)// string to correct
///             -> "All your BASE are ALL 2 us!!"             // corrected version
/// 
pub fn correct_string<T>(text: &String,
                         dict: &Vec<String>,
                         corrector: &mut T)
                         -> String
where T: Corrector                               // 3rd param must impl Corrector to have correct_word() function
{
  let re = Regex::new(r"[a-zA-Z\']+").unwrap();
  let mut ans = String::from("");
  let mut index = 0;
  
  for element in re.captures_iter(text) {
      //dbg!(&element);
      match element.get(0) {
          Some(d) => {
              let gap_string = &text[index..d.start()];
              let captial_element = d.as_str().to_uppercase();
              let index_pos = dict.iter().position(|r| *r == captial_element);
              
              ans.push_str(gap_string);
              match index_pos {
                  Some(_i) => {
                      ans.push_str(d.as_str());
                  },
                  _ => {
                      ans.push_str(&corrector.correct_word(&d.as_str()));
                  }
              }
              index = d.end();
          },
          _ => {
              
          }
      }
  }
  
  ans.push_str(&text[index..text.len()]);
  ans
}

////////////////////////////////////////////////////////////////////////////////
// Mark Corrector

/// This struct implements marking incorrect words with a begin/end
/// string pair so that they can identified and corrected later.
pub struct MarkCorrector {
  beg_mark: String,
  end_mark: String,
}

impl MarkCorrector {
  /// Create a MarkCorrector with the given begin/end markings
  pub fn new(beg_mark: &str, end_mark: &str) -> MarkCorrector{
    return MarkCorrector {beg_mark: beg_mark.to_string(), end_mark: end_mark.to_string(),}
  }
}

impl Corrector for MarkCorrector {
  /// Implementation of the correct_word() function to give
  /// MarkCorrector the Corrector trait. This function will return a
  /// given `word` with the begin/end marking strings prepended and
  /// appended and the word upcased. The format!() macro is useful for
  /// this.
  /// 
  /// EXAMPLES:
  /// let mut mc = MarkCorrector::new(">>","<<");
  /// mc.correct_word("incorrect") -> ">>INCORRECT<<"
  /// mc.correct_word("blergh") -> ">>BLERGH<<"
  /// 
  /// let mut mc = MarkCorrector::new("","!fixme");
  /// mc.correct_word("incorrect") -> "INCORRECT!fixme:"
  /// mc.correct_word("blergh") -> "BLERGH!fixme"
  fn correct_word(&mut self, word: &str) -> String{
    let mut ans = String::from("");
    ans.push_str(&self.beg_mark);
    ans.push_str(&(word.to_uppercase()));
    ans.push_str(&self.end_mark);
    return ans;
  }
}

////////////////////////////////////////////////////////////////////////////////
// AutoCorrector

/// This struct is implements an automatic corrector that selects the
/// closest dictionary word to a given word. The show_sub field
/// controls whether automatic subsitions are shown with (true) or
/// without (false) the original word.
pub struct AutoCorrector {
  dict_words: Vec<String>,
  show_sub: bool,
}

impl AutoCorrector {
  /// Create a new AutoCorrector with the given dictionary and
  /// show_sub value.  The dictionary is cloned during new() so that
  /// the AutoCorrector owns its own data. This simplifies ownership
  /// issues that would otherwise require lifetime annotations.
  pub fn new(dict: &Vec<String>, show_sub: bool) -> AutoCorrector{
    let new_autocorrector = AutoCorrector {
        dict_words: dict.clone(),
        show_sub: show_sub,
    };
    return new_autocorrector;
  }

  /// Iterates through the AutoCorrector's dict_words and finds finds
  /// the word with the lowest edit distance according to the
  /// edit_distance() function. The word passed in is upcased before
  /// calculating distances as the dictionary is expected to be all
  /// upcased words. If there are multiple strings that with the same
  /// edit distance to the given word, whichever one appears first in
  /// the dictionary is returned. Returns a pair of the
  /// (closest_word,distance). If dict_words is empty, this function
  /// returns a pair of ("",usize::MAX)
  ///
  /// EDIT DISTANCE: The edit_distance crate is listed as a dependency
  /// for this package and will be downloaded. It provides the
  /// edit_distance(a,b)->usize function which returns an unsigned
  /// integer measuring how many single character edits differentiate
  /// two strings passed in. This metrics is also referred to as the
  /// "Levenshtein distance" and requires the use of dynamic
  /// programming to calculate properly.
  /// 
  /// EXAMPLES: 
  /// let dict = vec!["ALL","BASE","ARE","YOUR","US"];          // should be String not &str
  /// let mut ac = AutoCorrector::new(&dict,false);             // use auto corrector
  /// ac.closest_word("bass")   -> ("BASE",1)
  /// ac.closest_word("belong") -> ("ALL",5)
  /// 
  /// let dict = vec!["A","B","C"];
  /// let mut ac = AutoCorrector::new(&dict,false);             
  /// ac.closest_word("a")   -> ("A",0)                         // in dictionary
  /// ac.closest_word("aa")  -> ("A",1)
  /// ac.closest_word("bbb") -> ("B",2)
  /// ac.closest_word("zz")  -> ("A",2)                         // alphabetic first
  /// 
  /// let dict = vec![];
  /// let mut ac = AutoCorrector::new(&dict,false);             // empty dictionary
  /// ac.closest_word("bass")   -> ("",18446744073709551615)
  /// ac.closest_word("belong") -> ("",18446744073709551615)
  pub fn closest_word(&self, word: &str) -> (String,usize) {
    if self.dict_words.len() == 0 {
      return ("".to_string(), usize::MAX);
    }
    let mut distance = usize::MAX;
    let mut string_ans = String::from("");
    let original_str = word.to_uppercase();

    for element in &self.dict_words {
      let dis = edit_distance(&original_str, &element);
      if dis < distance {
        distance = dis;
        string_ans = element.clone();
      }
    }
    return (string_ans, distance);
  }
}


impl Corrector for AutoCorrector {
  /// Implementation for Corrector. Uses closest_word() to find the
  /// closest dict_word to the given word. If the show_sub is true,
  /// returns a "verbose" correction that shows the original word,
  /// substituted word, and their edit distance in the format shown
  /// below. Otherwise, just returns the closest word found.
  ///
  /// EXAMPLES:
  /// let dict = vec!["ALL","BASE","ARE","YOUR","US"];
  ///
  /// let mut ac = AutoCorrector::new(&dict,false);       // show_sub is false
  /// ac.correct_word("bass") -> "BASE"                   // corrections are closest words    
  /// ac.correct_word("us") -> "US"
  /// ac.correct_word("belong") -> "ALL"
  // 
  /// let mut ac = AutoCorrector::new(&dict,true);        // show_sub is true
  /// ac.correct_word("bass") -> "(bass:BASE:1)"          // corrections include original
  /// ac.correct_word("us") -> "(us:US:0)"                // and closest word and edit
  /// ac.correct_word("belong") -> "(belong:ALL:5)"       // distance
  fn correct_word(&mut self, word: &str) -> String{
    if self.show_sub == true {
      let mut ans = String::from("(");
      ans.push_str(&word);
      let tuple = self.closest_word(word);
      ans.push_str(":");
      ans.push_str(&tuple.0);
      ans.push_str(":");
      ans.push_str(&tuple.1.to_string());
      ans.push_str(")");

      return ans;
    }
    let tuple = self.closest_word(word);
    return tuple.0;
  }
}

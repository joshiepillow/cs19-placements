use context starter2024
#flags from p1
vietnam = overlay(star(60, "solid", "yellow"), 
  rectangle(300, 200, "solid", "red"))

chile = above(beside(overlay(star(30, "solid", "white"), 
      rectangle(100, 100, "solid", "blue")), 
    rectangle(200, 100, "solid", "white")), 
  rectangle(300, 100, "solid", "red"))

suriname = above(above(above(above(
        rectangle(300, 40, "solid", "dark green"), 
        rectangle(300, 20, "solid", "white")), 
      overlay(star(45, "solid", "yellow"), 
        rectangle(300, 80, "solid", "dark red"))), 
    rectangle(300, 20, "solid", "white")), 
  rectangle(300, 40, "solid", "dark green"))

saint-lucia = overlay-xy(overlay-align("center", "bottom", 
    overlay-align("center", "bottom", 
      isosceles-triangle(98, 90, "solid", "yellow"), 
      isosceles-triangle(160, 45, "solid", "black")), 
    isosceles-triangle(180, 45, "solid", "white")), -80, -20, 
  rectangle(300, 200, "solid", "light blue"))

turkey = overlay-xy(beside(underlay-xy(
      circle(50, "solid", "white"), 18, 8, 
      circle(42, "solid", "red")), 
    rotate(30, star(25, "solid", "white"))), -60, -50, 
  rectangle(300, 200, "solid", "red"))


#functions from p2

# score-by-length :: List-of-strings -> Number
# adds together the lengths of all the strings in a list
fun score-by-length(string-list):
  foldl(lam(acc, string): acc + string-length(string) end, 0, string-list)
where:
  score-by-length(empty) is 0
  score-by-length([list: ""]) is 0
  score-by-length([list: "hello"]) is 5
  score-by-length([list: "hello", "world"]) is 10
  score-by-length([list: "", "1234567890!@#$%^&*()", " a "]) is 23
end

# words-to-sentence :: List-of-strings -> String
# combine a list of strings into a single string, adding spaces between words
fun words-to-sentence(string-list):
  if is-empty(string-list): ""
  else: 
    foldl(lam(next, acc): 
      next + " " + acc end, string-list.first, 
      string-list.rest)
  end
where:
  words-to-sentence(empty) is ""
  words-to-sentence([list: ""]) is ""
  words-to-sentence([list: "a"]) is "a"
  words-to-sentence([list: "Hello", "world"]) is "Hello world"
  words-to-sentence([list: "a ", " b  ", "  c   "]) is "a   b     c   "
end


#functions from p3

# valid-words :: List-of-strings List-of-chars -> List-of-strings
# filters a list of words, keeping a word if it only contains valid characters
fun valid-words(string-list, char-list):
  filter(lam(string): 
      is-empty(filter(lam(char): # keep if there are no invalid characters
        not(char-list.member(char)) end, 
        string-explode(string))) 
    end, string-list)
where:
  valid-words(empty, [list: 'a', 'b', 'c']) is empty
  valid-words(empty, empty) is empty
  valid-words([list: "abc", "ab", "a", "", "cd", "abcd", "dab", "0"], [list: "a", "b", "d"]) 
    is [list: "ab", "a", "", "dab"]
  valid-words([list: "abc", "ab", "a", "cd", "abcd", "dab", "0"], [list: "x",  "y", "z", "1", "2", "3", "4", "5"]) 
    is empty
  valid-words([list: "abc", "ab", "a", "cd", "abcd", "dab", "00000000000000000000"], [list: "x", "y", "z", "1", "2", "3", "4", "0"]) 
    is [list: "00000000000000000000"]
end

# unique :: List-of-any -> List-of-any
# given a list, returns the list without duplicate entries, keeping the first occurance of any element
fun unique(any-list):
  foldl(lam(acc, next): link(next, acc) end, empty, # reverse order
    foldl(lam(acc, next):
        if acc.member(next): # add element only if not already in accumulated
          acc
        else:
          link(next, acc) 
        end
      end, 
      empty, any-list))
where:
  unique([list: 1, 2, 3, 4, 5, 4, 3, 2, 1, "3"]) 
    is [list: 1, 2, 3, 4, 5, "3"]
  unique(empty) is empty
  unique([list: 1, 2, 3, 4, 5, "3"]) is [list: 1, 2, 3, 4, 5, "3"]
  unique([list: true, true, true, true, true, true]) is [list: true]
  unique([list: 1, 2, 3, 4, 5, 4, 3, 2, 1, 1, 1, 2, 2, 2, 3, 3, 3, 6, 0, 6, 7, 8, 0]) 
    is [list: 1, 2, 3, 4, 5, 6, 0, 7, 8]
end


#function from p4

# l33t :: List-of-strings -> List-of-strings
# given a list of strings, returns the list with certain characters in each string replaced
fun l33t(string-list):
  map(lam(string): # apply to each string
      foldl(_plus, "", # concatenate resulting chars into str
        map(lam(char): # apply to each char
            if ((char == 'A') or (char == 'a')):
              "4"
            else if ((char == 'E') or (char == 'e')):
              "3"
            else if ((char == 'I') or (char == 'i')):
              "1"
            else if ((char == 'O') or (char == 'o')):
              "0"
            else:
              char
            end
          end,
          string-explode(string)))
    end,
    string-list)
where:
  l33t(empty) is empty
  l33t([list: "hello"]) is [list: "h3ll0"]
  l33t([list: "aeiouAEIOU AEIOUaeiou"]) is [list: "4310u4310U 4310U4310u"]
  l33t([list: "cat", "abc", "a", "CEO", "aaabbbbbdddde", "dacbO", ""]) 
    is [list: "c4t", "4bc", "4", "C30", "444bbbbbdddd3", "d4cb0", ""]
  l33t([list: "much", "lurch", "123456789", "", " : ", "bcdfghjklmnpqrstuvwxyzBCDFGHJKLMNPQRSTUVWXYZ"]) 
    is [list: "much", "lurch", "123456789", "", " : ", "bcdfghjklmnpqrstuvwxyzBCDFGHJKLMNPQRSTUVWXYZ"]
end


#new functions

# unique-r :: List-of-any -> List-of-any
# given a list, returns the list without duplicate entries, keeping the last occurance of any element
fun unique-r(any-list):
  foldr(lam(acc, next): 
      if acc.member(next):
        acc
      else:
        link(next, acc) 
      end
    end, 
    empty, any-list)
where:
  unique-r([list: 1, 2, 3, 4, 5, 4, 3, 2, 1, "3"]) 
    is [list: 5, 4, 3, 2, 1, "3"]
  unique-r(empty) is empty
  unique-r([list: 1, 2, 3, 4, 5, "3"]) is [list: 1, 2, 3, 4, 5, "3"]
  unique-r([list: true, true, true, true, true, true]) is [list: true]
  unique-r([list: 1, 2, 3, 4, 5, 4, 3, 2, 1, 1, 1, 2, 2, 2, 3, 3, 3, 6, 0, 6, 7, 8, 0]) 
    is [list: 5, 4, 1, 2, 3, 6, 7, 8, 0]
end

# remove-one :: Number, List-of-number -> List-of-number
# given a list of numbers, remove the last occurance of a given element in the list if it exists
fun remove-one(value, num-list):
  if not(member(num-list, value)):
    num-list
  else:
    if not(member(num-list.rest, value)): # remove if this is last occcurance
      num-list.rest
    else:
      link(num-list.first, remove-one(value, num-list.rest))
    end
  end
where:
  remove-one(1, empty) is empty
  remove-one(1, [list: 1]) is empty
  remove-one(1, [list: 1, 1]) is [list: 1]
  remove-one(3, [list: 5, 4, 3, 2, 1, 2, 3, 4, 5]) 
    is [list: 5, 4, 3, 2, 1, 2, 4, 5]
  remove-one(5, [list: 1, 2, 3]) is [list: 1, 2, 3]
end

# median :: List-of-number -> number
# given a list of numbers, finds the value of its median
fun median(num-list):
  if is-empty(num-list.rest):
    num-list.first
  else if is-empty(num-list.rest.rest):
    (num-list.first + num-list.rest.first) / 2
  else:
    block:
      large = foldl(lam(a, b): if a > b: a else: b end end, num-list.first, num-list.rest)
      small = foldl(lam(a, b): if a < b: a else: b end end, num-list.first, num-list.rest)
      median(remove-one(small, remove-one(large, num-list)))
    end
  end
where: # given in problem that input is non-empty list
  median([list: 1]) is 1
  median([list: 1, 3]) is 2
  median([list: 3, 4, 5, 4, 9, 11, 0]) is 4
  median([list: 5, 4, 3, 2, -10000]) is 3
  median([list: 1, 1, 1, 1, 2, 1, 1, 2, 2]) is 1
end
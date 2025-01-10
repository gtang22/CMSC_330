import re
from functools import reduce

reverseCodonDict = {}
codonsDict = {}
evalsDict = {}

def read_codons(codon_file):
  # Regex to find all the valid codons
 
  valid_codon_sequence = r"^([A-Z]+[a-zA-Z]*): ((([AGCU]+\{\d+\}*)*[AGCU]*)(, (([AGCU]+\{\d+\}*)*[AGCU]*))*)$"
  
  codonsDict.clear()
  reverseCodonDict.clear()
  # This will open a file with a given path (codon_file)
  file = open(codon_file)
  lines = file.readlines()
  file.close()

  # Iterates through a file, storing each line in the line variable if the line 
  # is valid
  for line in lines:
      reg_match = re.match(valid_codon_sequence, line.strip())
      # If string is valid
      if reg_match is not None:
        codons = []
        stringList = reg_match.group(2).split(", ")
        is_valid = True
        # print(stringList)
        for codon in stringList:
          temp = translate(codon)
          if len(temp) > 0:
            codons.append(temp)
            reverseTuple = {temp: reg_match.group(1)}
            reverseCodonDict.update(reverseTuple)
          else:
            is_valid = False

        if is_valid:
          tempTuple = {reg_match.group(1): codons}
          codonsDict.update(tempTuple)

# Returns number from in regex
def getNumber(str):
   match = re.search(r'\d+', str)
   return int(match.group())

# Translate string with regex into just string
def translate(string):
  ans = ""
  while len(string) > 0:
      # Finds first regex of {num}
      match = re.search(r'{\d+}', string)
      if match:
        # Append substring before the match to result 
        ans = ans + string[0:match.start()];

        # Repeat the letter based on match
        repeatNumber = getNumber(match.group())
        if repeatNumber > 0:
          letter = string[match.start() - 1]
          for i in range(1, repeatNumber):
              ans = ans + letter
        else:
          return ""
        # Get rest substring
        string = string[match.end():len(string)]
      else:
         # No match, append the whole string into result
         ans = ans + string
         string = ""

  return ans   

def read_evals(eval_file):
  # This will open a file with a given path (eval_file)
  file = open(eval_file)
  lines = file.readlines()
  file.close()
  
  evalsDict.clear()

  regex = r"(\w+): ([LR], (PO|PR|I))"
  # Iterates through a file, storing each line in the line variable
  for line in lines:
    reg_match = re.match(regex, line.strip())
    # If the line is valid
    if reg_match is not None:
      # Splits the instructions into an list
      evals = reg_match.group(2).split(", ")
      # Put in the the instructions into the dictionary
      temp_tuple = {reg_match.group(1): evals}
      evalsDict.update(temp_tuple)

  print(evalsDict)

def encode(sequence):
  encodedSequence = ""
  # Splits the string into stringList
  stringList = sequence.split()

  # For every codon in stringList
  for string in stringList:
    # If the codon does exist in the codon list we made
    if codonsDict.get(string) is not None:
      # Gets the longest sequence of the codon
      codon = find_longest_codon(codonsDict.get(string))
      # Add to the end of encodedSequence
      encodedSequence = encodedSequence + codon

  # return the encodedSequence
  return encodedSequence

# Goes through a list and returns the longest string value
def find_longest_codon(list):
  longest = ""

  for x in list:
    if len(x) > len(longest):
      longest = x

  return longest

# Finds the longest key length
def find_longest_key_length():
  count = 0

  for key in reverseCodonDict.keys():
    if len(key) > count:
      count = len(key)

  return count

# Finds shortest key length
def find_shortest_key_length():
  count = 1000000

  for key in reverseCodonDict.keys():
    if len(key) < count:
      count = len(key)

  return count

# Regular decode, takes in sequence and decodes 
def decode(sequence):
  # Gets array of decoded sequence
  tempList = decode1(sequence)

  ans = ""
  for item in tempList:
    name = reverseCodonDict.get(item)
    # If is first codon
    if ans == "":
      ans = name
    else:
      ans = ans + " " + name

  return ans

# Different decode method, returns an array instead
def decode1(sequence):
  minLen = find_shortest_key_length()
  maxLen = find_longest_key_length()
  ans = []

# While string can have a codon in it
  while len(sequence) >= minLen:
      # Skips to the biggest codon the string can have
      if len(sequence) < maxLen:
        maxLen = len(sequence)

      found = False
      # Will first use longest string and work until the smallest
      for i in range(maxLen, minLen - 1, -1):
        codon = reverseCodonDict.get(sequence[0:i])
        if codon:
          ans.append(sequence[0:i])
          sequence = sequence[i:]
          found = True
          break

      # Start over from next letter
      if not found:
        sequence = sequence[1:]

  return ans

# Operate method
def operate(sequence, eval_name):
  # Get the evaluations for sequence
  evalProperties = evalsDict.get(eval_name)

  if evalProperties is None:
    return None
  
  ans = ""
  decodedString = sequence
  # If to read from right to left, reverse string
  if evalProperties[0] == "R":
    decodedString = sequence[::-1]

  print(f"==> {decode(decodedString)} --- {evalProperties[1]}")

  # Get an array of the translated string
  translatedArray = decode1(decodedString)
  if len(translatedArray) == 0:
    return ""

  flag = 0
  simplifiedList = []
  # Go through entire array and onyl get elements btw START and STOP
  for i in range(len(translatedArray)):
    if reverseCodonDict.get(translatedArray[i]) == "STOP":
      flag = 0
    elif reverseCodonDict.get(translatedArray[i]) == "START":
      flag = 1
    elif flag == 1:
      simplifiedList.append(translatedArray[i])

  print(f"decoded codon list: {simplifiedList}")
  # Operate on array
  if evalProperties[1] == "PR":
    ans = operate_pre(simplifiedList)
  elif evalProperties[1] == "PO":
    ans = operate_post(simplifiedList)
  else: 
    ans = operate_in(simplifiedList, "I")

  return ans

def operate_post(simplifiedList):
  codonStack = []
  top = -1
  for index in range(len(simplifiedList)):
    if is_codon(simplifiedList[index]):
      codonStack.append(simplifiedList[index])
      top += 1
    else:
      if reverseCodonDict.get(simplifiedList[index]) == "DEL":
        if top >= 0:
          codonStack.pop()
          top -= 1
      elif reverseCodonDict.get(simplifiedList[index]) == "SWAP":
        if top >= 1:
          temp = codonStack[top]
          codonStack[top] = codonStack[top - 1]
          codonStack[top - 1] = temp;
      elif reverseCodonDict.get(simplifiedList[index]) == "EXCHANGE":
        if top >= 0:
          codonStack[top] = different_codon(codonStack[top])
  
  ans = ""
  for item in codonStack:
    ans += item

  return ans

def operate_pre(simplifiedList):
  codonStack = []
  top = -1
  for index in range(len(simplifiedList)-1, -1, -1):
    if is_codon(simplifiedList[index]):
      codonStack.append(simplifiedList[index])
      top += 1
    else:
      if reverseCodonDict.get(simplifiedList[index]) == "DEL":
        if top >= 0:
          codonStack.pop()
          top -= 1
      elif reverseCodonDict.get(simplifiedList[index]) == "SWAP":
        if top >= 1:
          temp = codonStack[top]
          codonStack[top] = codonStack[top - 1]
          codonStack[top - 1] = temp;
      elif reverseCodonDict.get(simplifiedList[index]) == "EXCHANGE":
        if top >= 0:
          codonStack[top] = different_codon(codonStack[top])
  
  ans = ""
  for idx in range(len(codonStack)-1, -1, -1):
    ans += codonStack[idx]

  return ans

# Operate on array
def operate_in(simplifiedList, order):
  while len(simplifiedList) > 0:
    for index in range(len(simplifiedList)):
      if len(simplifiedList[index]) > 0:
        if reverseCodonDict.get(simplifiedList[index]) == "DEL":
          delete_in(simplifiedList, index)
        elif reverseCodonDict.get(simplifiedList[index]) == "SWAP":
          swap_in(simplifiedList, index)
        elif reverseCodonDict.get(simplifiedList[index]) == "EXCHANGE":
          exchange_in(simplifiedList, index)
    
    remove_inner_op_if_necessary(simplifiedList)

    tempList = []
    for item in simplifiedList:
      if item != "":
        tempList.append(item)

    if len(simplifiedList) == len(tempList):
      break
    
    simplifiedList = tempList

  ans = ""
  for item in simplifiedList:
    ans += item

  return ans

def remove_inner_op_if_necessary(simplifiedList):
    tempList = []
    for item in simplifiedList:
      if item != "":
        tempList.append(item)

    if len(simplifiedList) == len(tempList):
      for idx in range(len(simplifiedList)-1, 0, -1):
        if not is_codon(simplifiedList[idx]):
          simplifiedList[idx] = ""
          break

    return simplifiedList

# Returns if the given is a codon or not
def is_codon(codon):
  if codon == "":
    return False
  temp = reverseCodonDict.get(codon)
  return temp != "SWAP" and temp != "DEL" and temp != "EXCHANGE"

# Get different codon
def different_codon(codon):
  codonName = reverseCodonDict.get(codon)
  list = codonsDict.get(codonName)
  if codon != list[0] or len(list) == 1:
    return list[0]
  
  return list[1]

# Method for DEL
def delete_in(arr, index):
  # If no space to do DEL
  if index >= len(arr) - 1:
    arr[index] = ""
  # If element next to it isn't EXCHANGE, SWAP, or DEL
  elif is_codon(arr[index + 1]):
      arr[index + 1] = ""
      arr[index] = ""

# Method for SWAP
def swap_in(arr, index):
  # If no space
  if index <= 0 or index >= len(arr) - 1:
    arr[index] = ""
  elif is_codon(arr[index + 1]) and is_codon(arr[index - 1]):
    temp = arr[index + 1]
    arr[index + 1] = arr[index - 1]
    arr[index - 1] = temp
    arr[index] = ""

# Method for EXCHANGE
def exchange_in(arr, index):
    # If no space
    if index >= len(arr) - 1:
      arr[index] = ""
    elif is_codon(arr[index + 1]):
        arr[index + 1] = different_codon(arr[index + 1])
        arr[index] = ""


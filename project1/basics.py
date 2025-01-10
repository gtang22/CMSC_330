def isPalindrome(n):
  if n is None:
    return False
  else:
    string_n = str(n)
    left = 0
    right = len(string_n) - 1

    while left != right:
      if string_n[left] != string_n[right]:
        return False
      
      left += 1
      right -= 1

  return True

def nthmax(n, a):
  if n >= len(a) or n < 0:
    return None
  else:
    ans = 0
    temp_arr = a.copy()
    temp_arr.sort()
    ans = temp_arr[len(temp_arr) - n - 1]

  return ans

def freq(s):
  if len(s) == 0:
    return ""
  else:
    most  = ""
    most_count = 0

    for i in s:
      if s.count(i) > most_count:
        most = i
        most_count = s.count(i)
    return most


def zipHash(arr1, arr2):
  if arr1 is None or arr2 is None:
    return None
  else:
    if len(arr1) != len(arr2):
      return None
    else:
      hash = dict()
      arr_pos = 0

      for i in arr1:
        hash.update({i : arr2[arr_pos]})
        arr_pos += 1

    return hash 

def hashToArray(hash):
  if hash is None:
    return None
  else:
    arr = []
    result = hash.items()
    
    for k,v in result:
      arr.append([k,v])

    return arr

# Make a tree of every possible combination, with the lambdas either off or on

def maxLambdaChain(init, lambdas):
  if (len(lambdas) == 0):
    return init
  
  # If LA(LB(X)) is not possible
  temp_2 = maxLambdaChain(lambdas[0](init), lambdas[1:])
  temp = maxLambdaChain(init, lambdas[1:])
  

  return max(temp, temp_2)

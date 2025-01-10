def uniq(lst):
  if lst is None:
    return None
  else:
    hash = dict()
    ans_arr = []

    for i in lst:
      if hash.get(i) is None:
        hash.update({i: 1})
        ans_arr.append(i)

    return ans_arr

def find_max(matrix):
  if matrix is None:
    return None
  else:
    maximum = max(map(max, matrix))
    return maximum
  
def count_ones(matrix):
  if matrix is not None:
    amount = sum(map(lambda l: l.count(1), matrix))
    return amount

def addgenerator(x):
  if x is not None:
   return lambda y: y + x

def apply_to_self():
  return lambda x, y: x + y(x)

def map2(matrix,f):
  if matrix and f is not None:
    return list(map(lambda l: list(map(f, l)), matrix))

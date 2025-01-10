import re
from functools import reduce

class Fsm:
  def __init__(self,alphabet,states,start,final,transitions):
    self.sigma = alphabet
    self.states = states
    self.start = start
    self.final = final
    self.transitions = transitions
  def __str__(self):
    sigma = "Alphabet: " + str(self.sigma) + "\n"
    states = "States: " + str(self.states) + "\n"
    start = "Start: " + str(self.start) + "\n"
    final = "Final: " + str(self.final) + "\n"
    trans_header = "Transitions: [\n"
    thlen = len(trans_header)
    translist = ""
    for t in self.transitions:
      translist += " " * thlen + str(t)+ "\n"
    translist += " " * thlen + "]"
    transitions = trans_header + translist
    ret = sigma + states + start + final + transitions 
    return ret


# Global count
count = 0

# Increments count
def fresh(): 
  global count
  count += 1
  return count

# Makes a fsm that only accepts the given character
def char(string):
  finalFsm = None

  if string:
    # increment count
    s0 = fresh()
    # increment count again
    s1 = fresh()

    finalFsm = Fsm([string], [s0, s1], s0, [s1], [(s0, string, s1)])

  return finalFsm

# Combines the two fsms to take both in order of r1 -> r2
def concat(r1,r2):
  if not r1:
    return r2
  if not r2:
    return r1
 
  # gets r1's alphabet (array)
  finalFsmAlphabet = r1.sigma

  # adds in elements that r2 has (no duplicates)
  for char in r2.sigma:
    if not char in r1.sigma:
      finalFsmAlphabet.append(char)

  # combine states (both arrays)
  finalFsmStates = r1.states + r2.states
  # start state is r1's start state (single int)
  finalFsmStartState = r1.start
  # start state is r2's final state (array)
  finalFsmFinalState = r2.final

  # combine transitions (both arrays)
  finalFsmTransitions = r1.transitions + r2.transitions

  # 
  for finalState in r1.final:
    finalTuple = (finalState, 'epsilon', r2.start)
    finalFsmTransitions.append(finalTuple)

  finalFsm = Fsm(finalFsmAlphabet, finalFsmStates, finalFsmStartState, finalFsmFinalState, finalFsmTransitions)

  return finalFsm

def union(r1,r2):
  if not r1:
    return r2
  if not r2:
    return r1

  finalFsmAlphabet = r1.sigma

  for char in r2.sigma:
    if not char in r1.sigma:
      finalFsmAlphabet.append(char)

  newStartState = fresh()
  newFinalState = fresh()
  finalFsmStates = r1.states + r2.states + [newStartState, newFinalState]
  finalFsmStartState = newStartState
  finalFsmFinalState = [newFinalState]

  finalFsmTransitions = r1.transitions + r2.transitions
  finalFsmTransitions.append((newStartState, 'epsilon', r1.start))
  finalFsmTransitions.append((newStartState, 'epsilon', r2.start))

  for r1FinalState in r1.final:
    tempTuple = (r1FinalState, 'epsilon', newFinalState)
    finalFsmTransitions.append(tempTuple)

  for r2FinalState in r2.final:
    tempTuple = (r2FinalState, 'epsilon', newFinalState)
    finalFsmTransitions.append(tempTuple)

  finalFsm = Fsm(finalFsmAlphabet, finalFsmStates, finalFsmStartState, finalFsmFinalState, finalFsmTransitions)

  return finalFsm

def star(r1):
  if not r1:
    return None

  newStartState = fresh()
  newFinalState = fresh()
  finalFsmStates = r1.states + [newStartState, newFinalState]
  finalFsmTransitions = r1.transitions

  for r1FinalState in r1.final:
    tempTuple = (r1FinalState, 'epsilon', newFinalState)
    finalFsmTransitions.append(tempTuple)
  
  finalFsmTransitions.append((newStartState, 'epsilon', r1.start))
  finalFsmTransitions.append((newStartState, 'epsilon', newFinalState))
  finalFsmTransitions.append((newFinalState, 'epsilon', newStartState))

  finalFsm = Fsm(r1.sigma, finalFsmStates, newStartState, [newFinalState], finalFsmTransitions)

  return finalFsm

def move_inclusive(c,s,nfa):
  ansList = s.copy()
  transList = nfa.transitions

  # If the given character is in the nfa's alphabet
  if in_alphabet(c, nfa):
    for transition in transList:
      for state in s:
        if transition[0] == state and transition[1] == c:
          if not transition[2] in ansList:
            ansList.append(transition[2])

  
  return ansList

# Assume initial states are valid
# Probably do recursively
def e_closure(s,nfa):
  if not s or not nfa:
    return []

  # Gets Direct eplison edges for each state
  ansList = move_inclusive('epsilon', s, nfa)

  if len(ansList) > len(s):
    ansList = e_closure(ansList, nfa)

  # Return ansList
  return ansList

# Assume initial states are valid
# Doesn't account for epsilon edges yet
def move(c,s,nfa):
  if not s or not nfa:
    return []

  ansList = []
  transList = nfa.transitions

  # If the given character is in the nfa's alphabet
  if in_alphabet(c, nfa):
    for transition in transList:
      for state in s:
        if transition[0] == state and transition[1] == c:
          if not transition[2] in ansList:
            ansList.append(transition[2])

  
  return ansList

def in_alphabet(char, nfa):
  if nfa.sigma.count(char) != 0 or char == 'epsilon':
    return True
  return False


def normalize(state):
  finalState = state

  if isinstance(state, list):
    if len(state) > 1:
      finalState.sort()
    elif len(state) == 1:
      finalState = state[0]

  return finalState

def nfa_to_dfa(nfa): 
  if not nfa:
    return None

  dfaAlphabet = nfa.sigma
  dfaStart = normalize(e_closure([nfa.start], nfa))
  dfaTransitions = []

  states = [dfaStart]
  index = 0

  while True:
    for char in dfaAlphabet:
      tempStateList = []
      if isinstance(states[index], list):
        tempStateList = move(char, states[index], nfa)
      else:
        tempStateList = move(char, [states[index]], nfa)
        
      tempStateList = normalize(e_closure(tempStateList, nfa))

      if tempStateList != []:
          dfaTransitions.append((states[index], char, tempStateList))

      if not tempStateList in states and tempStateList != []:
        states.append(tempStateList)

    index += 1
    if index >= len(states):
      break

  dfaFinal = []
  
  for state in states:
    if isinstance(state, list):
      for s in state:
        if s in nfa.final:
          dfaFinal.append(state)
          break
    else:
      if state in nfa.final:
        dfaFinal.append(state)
        
  dfa = Fsm(dfaAlphabet, states, dfaStart, dfaFinal, dfaTransitions)
  return dfa

# accept method
def accept(nfa,string):
  if not nfa:
    return False

  ans = False
  currentStates = [nfa.start]
  currentStates = e_closure(currentStates, nfa)
  while len(string) > 0:
    currentStates = move(string[0], currentStates, nfa)
    print(f'before {currentStates}')
    currentStates = e_closure(currentStates, nfa)
    print(f'after {currentStates}')
    string = string[1:]



  for state in currentStates:
    if state in nfa.final:
      ans = True

  return ans

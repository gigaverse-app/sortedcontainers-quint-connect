// SortedList as seen through its public API: a multiset of integers kept in ascending order,
// with positional reads and pops that accept negative positions, and errors for a missing value
// or an out-of-range position. `event` names each step for the driver; `observed` is what the
// driver reads back from the real list after it.
module sorted_list {
  // What the last operation returned: a value, an IndexError, a ValueError, or nothing.
  type Result = Returned(int) | OutOfRange | Missing | NoResult
  type Event = { op: str, value: int, position: int, start: List[int] }
  type View = { contents: List[int], result: Result }

  var event: Event
  var observed: View

  pure def insertSorted(xs: List[int], value: int): List[int] =
    xs.select(x => x <= value).append(value).concat(xs.select(x => x > value))

  // Removes one copy of `value`; equal integers are indistinguishable, so which copy does not matter.
  pure def removeOne(xs: List[int], value: int): List[int] = {
    val same = xs.select(x => x == value)
    if (same.length() == 0) xs
    else xs.select(x => x < value).concat(same.slice(1, same.length())).concat(xs.select(x => x > value))
  }

  // Python positions: -1 is the last element.
  pure def absolute(xs: List[int], position: int): int = if (position < 0) position + xs.length() else position

  pure def inRange(xs: List[int], position: int): bool = {
    val index = absolute(xs, position)
    0 <= index and index < xs.length()
  }

  pure def dropAt(xs: List[int], index: int): List[int] = xs.slice(0, index).concat(xs.slice(index + 1, xs.length()))

  // SortedList(start): the constructor sorts its input.
  action create(start: List[int]): bool = all {
    event' = { op: "create", value: 0, position: 0, start: start },
    observed' = { contents: start.foldl([], (acc, x) => insertSorted(acc, x)), result: NoResult },
  }

  action init = {
    nondet start = Set([], [2], [3, 0, 2, 1], [3, 0, 2, 0, 1, 3, 2]).oneOf()
    create(start)
  }

  // sl.add(value)
  action add(value: int): bool = all {
    event' = { op: "add", value: value, position: 0, start: [] },
    observed' = { contents: insertSorted(observed.contents, value), result: NoResult },
  }

  // sl.discard(value): removes one copy if present, otherwise does nothing.
  action discard(value: int): bool = all {
    event' = { op: "discard", value: value, position: 0, start: [] },
    observed' = { contents: removeOne(observed.contents, value), result: NoResult },
  }

  // sl.remove(value): removes one copy, or raises ValueError and changes nothing.
  action remove(value: int): bool = all {
    event' = { op: "remove", value: value, position: 0, start: [] },
    observed' = if (observed.contents.select(x => x == value).length() == 0) { ...observed, result: Missing }
      else { contents: removeOne(observed.contents, value), result: NoResult },
  }

  // sl.pop(position): removes and returns the value there, or raises IndexError and changes nothing.
  action popAt(position: int): bool = all {
    event' = { op: "popAt", value: 0, position: position, start: [] },
    observed' = if (not(inRange(observed.contents, position))) { ...observed, result: OutOfRange }
      else {
        val index = absolute(observed.contents, position)
        { contents: dropAt(observed.contents, index), result: Returned(observed.contents[index]) }
      },
  }

  // sl[position]: returns the value there, or raises IndexError. Never changes the contents.
  action readAt(position: int): bool = all {
    event' = { op: "readAt", value: 0, position: position, start: [] },
    observed' = if (not(inRange(observed.contents, position))) { ...observed, result: OutOfRange }
      else { ...observed, result: Returned(observed.contents[absolute(observed.contents, position)]) },
  }

  // A read-only step, so that a witness can end in an assertion without repeating an action.
  action inspect = all {
    event' = { op: "inspect", value: 0, position: 0, start: [] },
    observed' = observed,
  }

  action step = {
    nondet value = 0.to(3).oneOf()
    nondet position = (-6).to(6).oneOf()
    any { add(value), discard(value), remove(value), popAt(position), readAt(position) }
  }

  val contentsSorted = 0.to(observed.contents.length() - 2).forall(i => observed.contents[i] <= observed.contents[i + 1])

  // discard removes one copy of a duplicate, not all of them.
  run duplicatesStaySortedTest = create([]).then(add(2)).then(add(0)).then(add(2)).then(discard(2))
    .then(all { assert(observed.contents == [0, 2]), inspect })

  // remove of an absent value and pop past the end raise and change nothing; a negative position
  // counts from the end.
  run errorsChangeNothingTest = create([1, 0]).then(remove(3)).then(popAt(2)).then(popAt(-2))
    .then(all { assert(observed.result == Returned(0) and observed.contents == [1]), inspect })

  // A positional read after an add that does not split a sublist must see the added value.
  run readAfterAddTest = create([3, 0, 2, 0, 1, 3, 2]).then(readAt(4)).then(add(1)).then(readAt(5))
    .then(all { assert(observed.result == Returned(2) and observed.contents == [0, 0, 1, 1, 2, 2, 3, 3]), inspect })

  // A positional read after a discard that does not merge sublists must not see the removed value.
  run readAfterDiscardTest = create([3, 0, 2, 0, 1, 3, 2]).then(readAt(4)).then(add(1)).then(discard(1)).then(readAt(4))
    .then(all { assert(observed.result == Returned(2) and observed.contents == [0, 0, 1, 2, 2, 3, 3]), inspect })
}

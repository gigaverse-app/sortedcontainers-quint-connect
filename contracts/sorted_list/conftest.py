from collections.abc import Callable

import pytest
from pytest_quint_connect import HistoryContext
from sorted_list_driver import Result, SortedListDriver, View

from sortedcontainers import SortedList

# Sublists split above 2 * LOAD values and merge at LOAD // 2 or fewer, so a history of ten steps
# exercises the split, merge and positional-index code that the default load of 1000 never reaches.
LOAD = 2

NO_RESULT: Result = {'tag': 'NoResult'}


class DiscardRemovesAll(SortedList):
    def discard(self, value):
        while value in self:
            super().discard(value)


class RemoveMissingSilent(SortedList):
    def remove(self, value):
        self.discard(value)


class StaleIndexOnAdd(SortedList):
    def _expand(self, pos):
        before = list(self._index)
        super()._expand(pos)
        # No split happened, so the index was updated in place: undo that.
        if self._index:
            self._index[:] = before


class StaleIndexOnDelete(SortedList):
    def _delete(self, pos, idx):
        before = list(self._index)
        super()._delete(pos, idx)
        # No merge happened, so the index was updated in place: undo that.
        if self._index:
            self._index[:] = before


MUTANTS: dict[str, type[SortedList]] = {
    'discard-removes-all': DiscardRemovesAll,
    'remove-missing-silent': RemoveMissingSilent,
    'stale-index-on-add': StaleIndexOnAdd,
    'stale-index-on-delete': StaleIndexOnDelete,
}


class SortedListContract(SortedListDriver):
    """Applies model actions to a real SortedList and reads it back. No list logic lives here."""

    def __init__(self, list_type: type[SortedList]) -> None:
        self.list_type = list_type
        self.sorted_list = list_type()
        self.result: Result = NO_RESULT

    def create(self, start: list[int]) -> None:
        self.sorted_list = self.list_type(start)
        self.sorted_list._reset(LOAD)
        self.result = NO_RESULT

    def add(self, value: int) -> None:
        self.sorted_list.add(value)
        self.result = NO_RESULT

    def discard(self, value: int) -> None:
        self.sorted_list.discard(value)
        self.result = NO_RESULT

    def remove(self, value: int) -> None:
        try:
            self.sorted_list.remove(value)
        except ValueError:
            self.result = {'tag': 'Missing'}
        else:
            self.result = NO_RESULT

    def pop_at(self, position: int) -> None:
        try:
            self.result = {'tag': 'Returned', 'value': self.sorted_list.pop(position)}
        except IndexError:
            self.result = {'tag': 'OutOfRange'}

    def read_at(self, position: int) -> None:
        try:
            self.result = {'tag': 'Returned', 'value': self.sorted_list[position]}
        except IndexError:
            self.result = {'tag': 'OutOfRange'}

    def inspect(self) -> None:
        pass

    def observe(self) -> View:
        # Iteration walks the sublists directly; unlike sl[i], it never builds the positional index.
        return {'contents': list(self.sorted_list), 'result': self.result}


@pytest.fixture
def sorted_list_driver() -> Callable[[HistoryContext], SortedListContract]:
    return lambda context: SortedListContract(MUTANTS.get(context.mutant, SortedList))

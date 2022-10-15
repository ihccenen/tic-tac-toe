const gameBoard = (() => {
  const board = []
  const rowCombinations = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6],
  ]

  return { board, rowCombinations }
})()

const createPlayer = (name, char) => {
  const _player = {
    name,
    char,
  }

  const play = pos => {
    gameBoard.board[pos] = _player.char

    // change row index to player character
    gameBoard.rowCombinations.reduce((prev, curr) => {
      if (curr.includes(pos)) curr.splice(curr.indexOf(pos), 1, _player.char)

      prev.push(curr)

      return prev
    }, [])
  }

  return { play }
}

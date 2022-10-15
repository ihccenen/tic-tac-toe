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

  const restart = () => {
    board.splice(0)
    rowCombinations.splice(
      0,
      8,
      [0, 1, 2],
      [3, 4, 5],
      [6, 7, 8],
      [0, 3, 6],
      [1, 4, 7],
      [2, 5, 8],
      [0, 4, 8],
      [2, 4, 6]
    )
  }

  return { board, rowCombinations, restart }
})()

const gameFlow = (() => {
  const _history = {
    turnCount: 0,
    result: false,
  }

  const takeTurn = event => {
    const index = +event.target.dataset.cell
    const notValidTurn =
      gameBoard.board[index] !== undefined || _history.result !== false

    if (notValidTurn) return

    const itsEven = _history.turnCount % 2 === 0

    if (itsEven) players.X.play(index)
    else players.O.play(index)

    event.target.textContent = gameBoard.board[index]
    event.target.style.cursor = 'auto'
    _history.turnCount++
    _checkEnd()
  }

  const _checkEnd = () => {
    const rowFiltered = gameBoard.rowCombinations
      .map(subArr => subArr.join(''))
      .filter(str => /XXX/.test(str) || /OOO/.test(str))

    if (rowFiltered.length > 0) _history.result = rowFiltered.join('')[0]
    else if (_history.turnCount === 9) _history.result = 'draw'

    checkResult()
  }

  const checkResult = () => {
    if (_history.result === 'draw') {
      displayControl.announce.textContent = 'Draw'
    } else if (_history.result !== false) {
      displayControl.announce.textContent = `Winner: ${
        players[_history.result].name
      }`
    }
  }

  return { takeTurn }
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

  return { name, play }
}

const displayControl = (() => {
  const cells = Array.from(document.querySelectorAll('[data-cell]'))
  const announce = document.querySelector('[data-announce]')

  cells.forEach(cell => cell.addEventListener('click', gameFlow.takeTurn))

  return { announce }
})()

const players = {
  X: createPlayer('player1', 'X'),
  O: createPlayer('player2', 'O'),
}

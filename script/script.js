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
    displayControl.cells.forEach(cell => {
      cell.textContent = ''
      cell.style.cursor = 'pointer'
    })
    displayControl.announce.textContent = '\xa0'
    gameFlow.history.turnCount = 0
    gameFlow.history.result = false
  }

  return { board, rowCombinations, restart }
})()

const gameFlow = (() => {
  const history = {
    turnCount: 0,
    result: false,
  }

  const takeTurn = event => {
    const index = +event.target.dataset.cell
    const notValidTurn =
      gameBoard.board[index] !== undefined || history.result !== false

    if (notValidTurn) return

    const itsEven = history.turnCount % 2 === 0

    if (itsEven) players.X.play(index)
    else players.O.play(index)

    event.target.textContent = gameBoard.board[index]
    event.target.style.cursor = 'auto'
    history.turnCount++
    _checkEnd()
  }

  const _checkEnd = () => {
    const rowFiltered = gameBoard.rowCombinations
      .map(subArr => subArr.join(''))
      .filter(str => /XXX/.test(str) || /OOO/.test(str))

    if (rowFiltered.length > 0) history.result = rowFiltered.join('')[0]

    checkResult()
  }

  const checkResult = () => {
    if (history.result !== false) {
      displayControl.announce.textContent = `Winner: ${
        players[history.result].name
      }`

      return
    } else if (history.turnCount === 9) {
      displayControl.announce.textContent = 'Draw'
    }
  }

  return { history, takeTurn }
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

  return { name, char, play }
}

const players = {
  X: createPlayer('player1', 'X'),
  O: createPlayer('player2', 'O'),
}

const displayControl = (() => {
  const cells = Array.from(document.querySelectorAll('[data-cell]'))
  const playerName = document.querySelector('[data-name]')
  const playerChar = document.querySelector('[data-char]')
  const announce = document.querySelector('[data-announce]')
  const restart = document.querySelector('[data-button="restart"]')

  cells.forEach(cell => cell.addEventListener('click', gameFlow.takeTurn))
  restart.addEventListener('click', gameBoard.restart)
  playerName.textContent = players.X.name
  playerChar.textContent = players.X.char

  return { announce, cells }
})()

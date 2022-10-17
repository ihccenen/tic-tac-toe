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
    automaticPlay.autoCheck()
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

    if (itsEven && players.X.auto === false) players.X.play(index)
    else if (players.O.auto === false) players.O.play(index)

    automaticPlay.autoCheck()

    // display result if someone won or if it's a draw
    if (history.result !== false)
      displayControl.announce.textContent = history.result
  }

  return { history, takeTurn }
})()

const createPlayer = (name, char, auto) => {
  const play = pos => {
    gameBoard.board[pos] = char
    displayControl.cells[pos].textContent = gameBoard.board[pos]
    displayControl.cells[pos].style.cursor = 'auto'
    gameFlow.history.turnCount++

    // change row number that equals to pos
    gameBoard.rowCombinations.reduce((prev, curr) => {
      if (curr.includes(pos)) curr.splice(curr.indexOf(pos), 1, char)

      prev.push(curr)

      return prev
    }, [])

    checkEnd()
  }

  // check for a three row win or if it's round 9 and no one won yet
  const checkEnd = () => {
    const win = new RegExp(char.repeat(3))
    const threeRow = gameBoard.rowCombinations
      .filter(subArr => win.test(subArr.join('')))
      .flat()
      .join('')

    if (win.test(threeRow)) {
      gameFlow.history.result = `Winner: ${name}`

      return
    }

    if (gameFlow.history.turnCount === 9) {
      gameFlow.history.result = 'Draw'
    }
  }

  return { name, char, auto, play }
}

const players = {
  X: createPlayer('player1', 'X', false),
  O: createPlayer('CPU', 'O', true),
}

const displayControl = (() => {
  const openModal = () => {
    modal.showModal()
  }

  const changePlayer = event => {
    event.preventDefault()
    const name = document.querySelector('[data-input="name"]')
    const char = document.querySelector('[data-select]')
    const newPlayer = createPlayer(name.value, char.value.toUpperCase(), false)

    for (let key in players) {
      if (key === newPlayer.char) players[key] = newPlayer
      else players[key] = createPlayer('CPU', key, true)
    }

    players[newPlayer.char] = newPlayer
    playerName.textContent = newPlayer.name
    playerChar.textContent = newPlayer.char
    form.reset()
    modal.close()
    gameBoard.restart()
    automaticPlay.autoCheck()
  }

  const cells = Array.from(document.querySelectorAll('[data-cell]'))
  const playerName = document.querySelector('[data-name]')
  const playerChar = document.querySelector('[data-char]')
  const announce = document.querySelector('[data-announce]')
  const changeBtn = document.querySelector('[data-button="change"]')
  const modal = document.querySelector('[data-modal]')
  const form = document.querySelector('[data-form]')
  const restart = document.querySelector('[data-button="restart"]')

  cells.forEach(cell => cell.addEventListener('click', gameFlow.takeTurn))
  restart.addEventListener('click', gameBoard.restart)
  changeBtn.addEventListener('click', openModal)
  form.addEventListener('submit', changePlayer)
  playerName.textContent = players.X.name
  playerChar.textContent = players.X.char

  return { announce, cells }
})()

const automaticPlay = (() => {
  const _play = char => {
    const avaibleBoard = []

    gameBoard.rowCombinations
      .flat()
      .filter(item => isFinite(item))
      .map(item => {
        if (avaibleBoard.includes(item)) return

        avaibleBoard.push(item)
      })

    // check if the game already ended
    if (gameFlow.history.result !== false) return

    const randomIndex = Math.floor(Math.random() * avaibleBoard.length)

    players[char].play(avaibleBoard[randomIndex])
  }

  const autoCheck = () => {
    const itsEven = gameFlow.history.turnCount % 2 === 0
    if (players.X.auto === true && itsEven) _play('X')
    else if (players.O.auto === true && !itsEven) _play('O')
  }

  autoCheck()

  return { autoCheck }
})()

const gameBoard = (() => {
  const board = [0, 1, 2, 3, 4, 5, 6, 7, 8]

  const restart = () => {
    board.splice(0, board.length, 0, 1, 2, 3, 4, 5, 6, 7, 8)
    displayControl.cells.forEach(cell => {
      cell.textContent = ''
      cell.style.cursor = 'pointer'
    })
    displayControl.announce.textContent = '\xa0'
    gameFlow.history.turnCount = 0
    gameFlow.history.result = false
    automaticPlay.autoCheck()
  }

  return { board, restart }
})()

const gameFlow = (() => {
  const history = {
    turnCount: 0,
    result: false,
  }

  const takeTurn = event => {
    const index = +event.target.dataset.cell
    const notValidTurn =
      isNaN(gameBoard.board[index]) || history.result !== false

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

    _checkEnd()
  }

  const _checkEnd = () => {
    const threeRow = {
      horizontal: new RegExp(`^${char}{3}|.{3}${char}{3}.{3}|${char}{3}$`),
      vertical: new RegExp(
        `(${char}.{2}){3}|(.{1}${char}.{1}){3}|(.{2}${char}){3}`
      ),
      diagonal: new RegExp(
        `(${char}.{3}){2}${char}|.{2}${char}.{1}${char}.{1}${char}.{2}`
      ),
    }

    for (let regex in threeRow) {
      if (threeRow[regex].test(gameBoard.board.join(''))) {
        gameFlow.history.result = `Winner: ${name}`

        return
      }
    }

    if (gameFlow.history.turnCount === 9) gameFlow.history.result = 'Draw'
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
    const avaibleBoard = gameBoard.board.filter(item => isFinite(item))

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

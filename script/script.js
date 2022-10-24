;(function () {
  const gameBoard = (() => {
    const board = [0, 1, 2, 3, 4, 5, 6, 7, 8]

    const resetGame = () => {
      board.splice(0, board.length, 0, 1, 2, 3, 4, 5, 6, 7, 8)
      displayControl.resetDisplay()
      gameFlow.resetHistory()
      gameFlow.checkAutoPlay()
    }

    return { board, resetGame }
  })()

  const gameFlow = (() => {
    const history = {
      playerTurn: 'X',
      turnCount: 0,
      result: false,
    }

    const takeTurn = event => {
      const index = +event.target.dataset.cell
      // check if the cell can be played and theres no result
      const invalidTurn =
        isNaN(gameBoard.board[index]) || history.result !== false

      if (invalidTurn) return

      players[history.playerTurn].play(index)

      if (gameFlow.history.turnCount === 9) {
        gameFlow.history.result = 'Draw'
        displayControl.showResult()

        return
      }

      checkAutoPlay()
    }

    const checkAutoPlay = () => {
      if (players.X.auto && history.playerTurn === 'X') _autoPlay('X')
      else if (players.O.auto && history.playerTurn === 'O') _autoPlay('O')
    }

    const _autoPlay = char => {
      const depth = 9 - history.turnCount
      const isMaximizingPlayer = char === 'X' ? true : false
      const index = _minimax(
        gameBoard.board,
        depth,
        -Infinity,
        Infinity,
        isMaximizingPlayer
      )

      players[char].play(index.index)
    }

    const _minimax = (node, depth, alpha, beta, maximizingPlayer) => {
      if (checkWin('X', node)) return { score: 1 }
      else if (checkWin('O', node)) return { score: -1 }
      else if (depth === 0) return { score: 0 }

      const availableIndexes = node.filter(item => isFinite(item))
      const allPlays = []

      for (let index of availableIndexes) {
        if (maximizingPlayer) {
          node[index] = 'X'
          const result = _minimax(node, depth - 1, alpha, beta, false)

          node[index] = index
          allPlays.push({ index, score: result.score })

          if (result.score > beta) break

          alpha = Math.max(alpha, result.score)
        } else {
          node[index] = 'O'
          const result = _minimax(node, depth - 1, alpha, beta, true)

          node[index] = index
          allPlays.push({ index, score: result.score })

          if (result.score < alpha) break

          beta = Math.min(beta, result.score)
        }
      }

      let bestPlay = null

      if (maximizingPlayer) {
        let value = -Infinity

        for (let i = 0; i < allPlays.length; i++) {
          if (allPlays[i].score > value) {
            value = allPlays[i].score
            bestPlay = i
          }
        }
      } else {
        let value = Infinity

        for (let i = 0; i < allPlays.length; i++) {
          if (allPlays[i].score < value) {
            value = allPlays[i].score
            bestPlay = i
          }
        }
      }

      return allPlays[bestPlay]
    }

    const changePlayerTurn = () => {
      if (history.playerTurn === 'X') history.playerTurn = 'O'
      else history.playerTurn = 'X'
    }

    const checkWin = (char, board) => {
      // check horizontals wins
      if (
        (board[0] === char && board[1] === char && board[2] === char) ||
        (board[3] === char && board[4] === char && board[5] === char) ||
        (board[6] === char && board[7] === char && board[8] === char)
      ) {
        return true
      }
      // check verticals wins
      else if (
        (board[0] === char && board[3] === char && board[6] === char) ||
        (board[1] === char && board[4] === char && board[7] === char) ||
        (board[2] === char && board[5] === char && board[8] === char)
      ) {
        return true
      }
      // check diagonals wins
      else if (
        (board[0] === char && board[4] === char && board[8] === char) ||
        (board[2] === char && board[4] === char && board[6] === char)
      ) {
        return true
      }

      // if none pass the conditions return false
      return false
    }

    const resetHistory = () => {
      history.playerTurn = 'X'
      history.turnCount = 0
      history.result = false
    }

    return {
      history,
      takeTurn,
      resetHistory,
      checkAutoPlay,
      changePlayerTurn,
      checkWin,
    }
  })()

  const createPlayer = (name, char, auto) => {
    const play = pos => {
      gameBoard.board[pos] = char
      displayControl.showCell(char, pos)
      gameFlow.history.turnCount++
      gameFlow.changePlayerTurn()

      if (gameFlow.checkWin(char, gameBoard.board)) {
        _filterRows()
        gameFlow.history.result = `Winner: ${name}`
        displayControl.showResult(gameFlow.history.result)
      }
    }

    const _filterRows = () => {
      const rows = [
        [0, 1, 2],
        [3, 4, 5],
        [6, 7, 8],
        [0, 3, 6],
        [1, 4, 7],
        [2, 5, 8],
        [0, 4, 8],
        [2, 4, 6],
      ]

      // change row indexes to the board characters
      rows.reduce((prev, curr) => {
        for (let i = 0; i < curr.length; i++) {
          if (curr[i] === 'X' || curr[i] === 'O') continue

          curr[i] = gameBoard.board[curr[i]]
        }
        prev.push(curr)
        return prev
      }, [])

      // divide rows in directions
      const horizontal = rows.splice(0, 3)
      const vertical = rows.splice(0, 3)
      const diagonal = rows.splice(0, 2)

      // check which direction to show win line
      if (_checkWinRow(horizontal, 'horizontal')) return
      else if (_checkWinRow(vertical, 'vertical')) return
      else if (_checkWinRow(diagonal, 'diagonal')) return
    }

    const _checkWinRow = (arr, direction) => {
      for (let i = 0; i < arr.length; i++) {
        if (gameFlow.checkWin('X', arr[i]) || gameFlow.checkWin('O', arr[i])) {
          // send to where to begin and the direction
          displayControl.showWinLine(i + 1, direction)
        }
      }
    }

    return { name, char, auto, play }
  }

  const players = {
    X: createPlayer('player', 'X', false),
    O: createPlayer('CPU', 'O', true),
  }

  const displayControl = (() => {
    const showCell = (char, pos) => {
      cells[pos].textContent = char
      cells[pos].style.cursor = 'auto'
    }

    const showResult = () => {
      announce.textContent = gameFlow.history.result
    }

    const showWinLine = (row, direction) => {
      lineDiv.classList.toggle(direction)

      if (row === 1) lineDiv.classList.toggle('first-row')
      else if (row === 2) lineDiv.classList.toggle('second-row')
      else if (row === 3) lineDiv.classList.toggle('third-row')

      // needed so the line transition works
      setTimeout(() => {
        lineDiv.classList.toggle('stretch')
      }, 1)
    }

    const resetDisplay = () => {
      lineDiv.className = 'line'
      cells.forEach(cell => {
        cell.textContent = '\xa0'
        cell.style.cursor = 'pointer'
      })

      announce.textContent = '\xa0'
    }

    const openModal = () => {
      modal.showModal()
    }

    const changePlayer = event => {
      event.preventDefault()
      const name = document.querySelector('[data-input="name"]')
      const char = document.querySelector('[data-select]')
      const newPlayer = createPlayer(
        name.value,
        char.value.toUpperCase(),
        false
      )

      for (let key in players) {
        if (key === newPlayer.char) players[key] = newPlayer
        else players[key] = createPlayer('CPU', key, true)
      }

      players[newPlayer.char] = newPlayer
      playerName.textContent = newPlayer.name
      playerChar.textContent = newPlayer.char
      form.reset()
      modal.close()
      gameBoard.resetGame()
    }

    const cells = Array.from(document.querySelectorAll('[data-cell]'))
    const playerName = document.querySelector('[data-name]')
    const playerChar = document.querySelector('[data-char]')
    const lineDiv = document.querySelector('[data-line]')
    const announce = document.querySelector('[data-announce]')
    const changeBtn = document.querySelector('[data-button="change"]')
    const modal = document.querySelector('[data-modal]')
    const form = document.querySelector('[data-form]')
    const restart = document.querySelector('[data-button="restart"]')

    cells.forEach(cell => cell.addEventListener('click', gameFlow.takeTurn))
    restart.addEventListener('click', gameBoard.resetGame)
    changeBtn.addEventListener('click', openModal)
    form.addEventListener('submit', changePlayer)
    playerName.textContent = players.X.name
    playerChar.textContent = players.X.char

    return { cells, showCell, showResult, showWinLine, resetDisplay }
  })()
})()

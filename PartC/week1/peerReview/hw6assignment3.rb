class MyPiece < Piece
  All_My_Pieces = All_Pieces + [
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]),
    [
      [[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]],
      [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]],
    ],
    rotations([[0, 0], [0, 1], [-1, 0]]),
  ]

  Cheat_Piece = [[[0, 0]]]

  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end
end

class MyBoard < Board
  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
  end

  def next_piece
    super

    if @is_cheating
      @is_cheating = false
      @current_block = MyPiece.cheat_piece(self)
    else 
      @current_block = MyPiece.next_piece(self)
    end    
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position

    locations.each_with_index{|current, index|
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
    }

    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if not @is_cheating and @score >= 100
      @score -= 100
      @is_cheating = true
    end
  end
end

class MyTetris < Tetris
  def key_bindings
    super

    @root.bind('u', proc {
      @board.rotate_clockwise
      @board.rotate_clockwise
    })

    @root.bind('c', proc {@board.cheat})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
end

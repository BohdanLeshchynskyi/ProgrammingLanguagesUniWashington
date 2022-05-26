# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  All_My_Pieces = All_Pieces + [ rotations([[0, 0], [1, 0], [0, 1], [1, 1],[2, 1]]), # 5 square tail
  [[[0, 0], [-1, 0], [1, 0], [2, 0],[3, 0]], # 5 square long
   [[0, 0], [0, -1], [0, 1], [0, 2],[0, 3]]],
   rotations([[0, 1], [0, 0], [1, 1]])] # 3 square small fella

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheat_piece (board)
    MyPiece.new([[[0,0]]], board)
  end

end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    @cheat
  end

  def do_cheat
    if !game_over? and @game.is_running? and @score >= 100 and !@cheat
      @score = @score - 100
      @cheat = true
    end
  end

  def next_piece
    if !cheat
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    else
      @current_block = MyPiece.next_cheat_piece(self)
      @current_pos = nil
      @cheat = false
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    size_locations = locations.length - 1
    (0..size_locations).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def remove_filled
    (2..(@grid.size-1)).each{|num| row = @grid.slice(num);
      # see if this row is full (has no nil)
      if @grid[num].all?
        # remove from canvas blocks in full row
        (0..(num_columns-1)).each{|index|
          @grid[num][index].remove;
          @grid[num][index] = nil
        }
        # move down all rows above and move their blocks on the canvas
        ((@grid.size - num + 1)..(@grid.size)).each{|num2|
          @grid[@grid.size - num2].each{|rect| rect && rect.move(0, block_size)};
          @grid[@grid.size-num2+1] = Array.new(@grid[@grid.size - num2])
        }
        # insert new blank row at top
        @grid[0] = Array.new(num_columns);
        # adjust score for full flow
        @score += 10;
      end}
    self
  end
  
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings 
    super
    @root.bind('u' , proc {@board.rotate_180}) 
    @root.bind('c' , proc {@board.do_cheat}) 
  end

  def buttons
    super
    rotate_180 = TetrisButton.new('u', 'lightgreen'){@board.rotate_180}
        rotate_180.place(35, 50, 127, 501)

    cheat = TetrisButton.new('c', 'lightgreen'){@board.do_cheat}
        cheat.place(35, 50, 27, 501)
  end
end



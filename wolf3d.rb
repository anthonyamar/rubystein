#!/usr/bin/env ruby
require 'rubygems'
require 'gosu'

#require 'config'
module Config
  WINDOW_WIDTH  = 640
  WINDOW_HEIGHT = 480
  FULLSCREEN    = false
  FPS           = 60
  SUB_DIVISION  = 3
  AI_INVOCATIONS_PER_LOOP = 2
end

#require 'map'
#require 'config'
#require 'sprite'
#require 'door'

class Map
  Infinity = 1.0 / 0
  TEX_WIDTH  = 64
  TEX_HEIGHT = 64
  GRID_WIDTH_HEIGHT = 64
  HALF_GRID_WIDTH_HEIGHT = GRID_WIDTH_HEIGHT / 2
  MIN_HALF_GRID_WIDTH_HEIGHT = -HALF_GRID_WIDTH_HEIGHT
  
  attr_accessor :matrix
  attr_reader   :window
  attr_reader   :textures
  attr_accessor :props
  attr_accessor :players
  attr_accessor :items
  #attr_accessor :sprites
  attr_accessor :doors
  attr_reader   :width
  attr_reader   :height
  
  attr_reader :player_x_init
  attr_reader :player_y_init
  attr_reader :player_angle_init
  
  class Adder
    def initialize(map)
      @map = map
    end
    
    def prop(klass, x, y, *args, &block)
      prop = klass.new(@map.window, x * GRID_WIDTH_HEIGHT, y * GRID_WIDTH_HEIGHT, *args, &block)
      @map.props << prop
      prop
    end
    
    def item(klass, x, y, *args, &block)
      item = klass.new(@map.window, @map, x * GRID_WIDTH_HEIGHT, y * GRID_WIDTH_HEIGHT, *args, &block)
      @map.items << item
      item
    end
    
    def player(klass, x, y, *args, &block)
      player = klass.new(@map.window, @map, x * GRID_WIDTH_HEIGHT, y * GRID_WIDTH_HEIGHT, *args, &block)
      @map.players << player
      player
    end
  end
  
  # @require for i in 0...matrix_row_column.size:
  #   matrix_row_column[i].size == matrix_row_column[i+1].size
  def initialize(matrix_row_column, texture_files, player_x_init, player_y_init, player_angle_init, window)
    @matrix = matrix_row_column
    @width  = matrix_row_column[0].size
    @height = matrix_row_column.size
    
    @player_x_init     = player_x_init
    @player_y_init     = player_y_init
    @player_angle_init = player_angle_init
    
    @window = window
    @doors  = []
    
    @height.times do
      column = [nil] * @width
      @doors << column
    end
    
    row = 0
    while(row < @height)
      col = 0
      while(col < @width)
        if @matrix[row][col] == -1
          @doors[row][col] = Door.new
        end
        col += 1
      end
      row += 1
    end
    
    @textures = [nil]
    texture_files.each {|tex_file|
      pair = {}
      
      tex_file.each_pair {|tex_type, tex_path|
        pair[tex_type] = SpritePool::get(window, tex_path)
      }
      
      @textures << pair
    }
    @items   = []
    @players = []
    @props   = []
  end
  
  def add
    yield(Adder.new(self))
  end
  
  def sprites
    @items + @players + @props
  end
  
  def find_nearest_intersection(start_x, start_y, angle)
    hor_x, hor_y = find_horizontal_intersection(start_x, start_y, angle)
    ver_x, ver_y = find_vertical_intersection(start_x, start_y, angle)
    
    hor_r = Math.sqrt( (hor_x - start_x) ** 2 + (hor_y - start_y) ** 2 )
    ver_r = Math.sqrt( (ver_x - start_x) ** 2 + (ver_y - start_y) ** 2 )
    
    if hor_r < ver_r
      return :horizontal, hor_r, hor_x, hor_y
    else
      return :vertical, ver_r, ver_x, ver_y
    end
  end
  
  def find_horizontal_intersection(start_x, start_y, angle)
    # When the angle is horizontal, we will never find a horizontal intersection.
    # After all, the ray would then be considered parallel to any possible horizontal wall.
    return Infinity, Infinity if angle == 0 || angle == 180
    
    grid_y = (start_y / GRID_WIDTH_HEIGHT).to_i
    
    if(angle > 0 && angle < 180)
      # Ray facing upwards
      ay = (grid_y * GRID_WIDTH_HEIGHT) - 1
      #ay = 0 if ay < 0
    else
      # Ray facing downwards
      ay = ( grid_y + 1 ) * GRID_WIDTH_HEIGHT
      #ay = grid_y * GRID_WIDTH_HEIGHT if not on_map?(*Map.matrixify(ay, start_x))
    end
    
    ax = start_x + (start_y - ay) / Math.tan(angle * Math::PI / 180)
    
    #if not on_map?(*Map.matrixify(ay, ax))
    #  [Infinity, Infinity]
    #end
    
    #if(ax < 0 || ax >= Config::WINDOW_WIDTH || ay < 0 || ay >= Config::WINDOW_HEIGHT)
    #  [Infinity, Infinity]
    #end
    
    if(!hit?(ax, ay, angle, :horizontal))
      # Extend the ray
      return find_horizontal_intersection(ax, ay, angle)
    else
      
      column, row = Map.matrixify(ax, ay)
      
      if door?(row, column)
        half_grid = GRID_WIDTH_HEIGHT / 2
        dy = (angle > 0 && angle < 180) ? half_grid * -1 : half_grid

        door_offset = half_grid / Math::tan(angle * Math::PI / 180).abs
        door_offset *= -1 if angle > 90 && angle < 270
        
        return ax + door_offset, ay + dy
      else
        return ax, ay
      end
    end
  end
  
  def find_vertical_intersection(start_x, start_y, angle)
    if angle == 90 || angle == 270
      [Infinity, Infinity]
    else
      grid_x = (start_x / GRID_WIDTH_HEIGHT).to_i
        
      if(angle > 90 && angle < 270)
        # Ray facing left
        bx = (grid_x * GRID_WIDTH_HEIGHT) - 1
      else
        # Ray facing right
        bx = (grid_x + 1) * GRID_WIDTH_HEIGHT
      end
    
      by = start_y + (start_x - bx) * Math.tan(angle * Math::PI / 180)
    
      # If the casted ray gets out of the playfield, emit infinity.
      #if(bx < 0 || bx >= Config::WINDOW_WIDTH || by < 0 || by >= Config::WINDOW_HEIGHT)
      #  [Infinity, Infinity]
      #else
      
      #if not on_map?(*Map.matrixify(by, bx))
      #  [Infinity, Infinity]
      #end
      
        if(!hit?(bx, by, angle, :vertical))
          #Extend the ray
          find_vertical_intersection(bx, by, angle)
        else
          column, row = Map.matrixify(bx,by)
      
          if door?(row, column)
            dx = (angle > 90 && angle < 270) ? MIN_HALF_GRID_WIDTH_HEIGHT : HALF_GRID_WIDTH_HEIGHT

            door_offset = HALF_GRID_WIDTH_HEIGHT * Math::tan(angle * Math::PI / 180).abs
            door_offset *= -1 if angle > 0 && angle < 180
            
            [bx + dx, by + door_offset]
          else
            [bx, by]
          end
        end
      #end
    end
  end
  
  def texture_for(type, x, y, angle)
    column = (x / GRID_WIDTH_HEIGHT).to_i
    row    = (y / GRID_WIDTH_HEIGHT).to_i
    
    texture_id = @matrix[row][column]
    texture    = @textures[texture_id]
    
    if type == :horizontal && angle > 0 && angle < 180
      if door?(row, column)
        texture[:south][(x - @doors[row][column].pos) % TEX_WIDTH]
      else
        if texture_id == 0
          puts "#{type} -- #{x} -- #{y} -- #{angle}"
          return @textures[-1][:south][x % TEX_WIDTH]
        end
        texture[:south][x % TEX_WIDTH]
      end
    elsif type == :horizontal && angle > 180
      if door?(row, column)
        texture[:north][(x - @doors[row][column].pos) % TEX_WIDTH]
      else
        if texture_id == 0
          puts "North: #{type} -- #{x} -- #{y} -- #{angle}"
          return @textures[-1][:north][x % TEX_WIDTH]
        end
        
        texture[:north][(TEX_WIDTH - x) % TEX_WIDTH]
      end
    elsif type == :vertical && angle > 90 && angle < 270
      if door?(row, column)
        texture[:west][(y - @doors[row][column].pos) % TEX_HEIGHT]
      else
        if texture_id == 0
          puts "North: #{type} -- #{x} -- #{y} -- #{angle}"
          return @textures[-1][:north][y % TEX_WIDTH]
        end
        
        texture[:west][(TEX_HEIGHT - y) % TEX_HEIGHT]
      end
    elsif type == :vertical && angle < 90 || angle > 270
      if door?(row, column)
        texture[:east][(y - @doors[row][column].pos) % TEX_HEIGHT]
      else
        if texture_id == 0
          puts "East: #{type} -- #{x} -- #{y} -- #{angle}"
          return @textures[-1][:east][y % TEX_WIDTH]
        end
        
        texture[:east][y % TEX_HEIGHT]
      end
    end
  end
  
  def walkable?(row, column)
    on_map?(row, column) && (@matrix[row][column] == 0 || (door?(row, column) && @doors[row][column].open?))
  end
  
  def hit?(x, y, angle = nil, type = nil)
    column, row = Map.matrixify(x,y)
    
    if(angle && (type == :horizontal || type == :vertical) && door?(row, column))
      offset = (type == :horizontal) ? x : y
      offset_door = 0

      dx = (angle > 90 && angle < 270) ? MIN_HALF_GRID_WIDTH_HEIGHT : HALF_GRID_WIDTH_HEIGHT
      
      if type == :vertical
        offset_door = dx * Math::tan(angle * Math::PI / 180) * -1
      else        
        offset_door = dx / Math::tan(angle * Math::PI / 180).abs
      end
      
      offset_on_door = offset + offset_door
      offset_on_door %= GRID_WIDTH_HEIGHT
      
      if type == :horizontal
        @doors[row][column].pos <= offset_on_door
      elsif type == :vertical
        @doors[row][column].pos <= offset_on_door
      else
        !self.walkable?(row, column)
      end
    else
      !self.walkable?(row, column)
    end
  end
  
  def door?(row, column)
    on_map?(row, column) && @matrix[row][column] == -1
  end
  
  def get_door(row, column, angle)
    if door?(row + 1, column) && (angle > (270 - Player::HALF_FOV)) && (angle < (270 + Player::HALF_FOV))
      return @doors[row + 1][column]
    elsif door?(row - 1, column) && (angle > (90 - Player::HALF_FOV)) && (angle < (90 + Player::HALF_FOV))
      return @doors[row - 1][column]
    elsif door?(row, column + 1) && ( (angle > (360 - Player::HALF_FOV)) || (angle < Player::HALF_FOV) )
      return @doors[row][column + 1]
    elsif door?(row, column - 1) && (angle > (180 - Player::HALF_FOV)) && (angle < (180 + Player::HALF_FOV))
      return @doors[row][column - 1]
    end
    
    return nil
  end
  
  def on_map?(row, column)
    if row < 0 or column < 0
      false
    else
      row < self.width && column < self.height
    end
  end
  
  def self.matrixify(x, y)
    [(x / GRID_WIDTH_HEIGHT).to_i, (y / GRID_WIDTH_HEIGHT).to_i]
  end
end



class MapPool
  @@maps = []
  
  def self.get(window, n = 0)
    n = n.to_i
    if @@maps[n].nil?
      klass = eval("Level#{n}")
      
      @@maps[n] = klass.create(window)
    end
    
    @@maps[n]
  end
end




#require 'sound'
#require 'rubygems'
#require 'gosu'

SOUND_TO_TEXT = {
  'mein_spagetthicode.ogg'  => 'My spaghetti code!',
  'meine_magischen_qpc.ogg' => 'My magic quotes!',
  'meine_sql.ogg'   => 'My SQL injections!',
  'balloon.ogg'     => "Don't you want a... balloon?",
  'dog_bark.ogg'    => 'Bark!',
  'dog_cry.ogg'     => ':-(',
  'floating.ogg'    => 'They are all FLOATING!',
  'test_all_the_effing_time_is_lame.ogg' => "'Test all the fucking time' is lame.",
  'long live php.ogg'    => 'Long live PHP!',
  'my damn php life.ogg' => 'My damn PHP life!',
  'myphplife.ogg'        => 'My life for PHP!',
  'phpforever.ogg'       => 'PHP forever!',
  'omgponies.ogg'        => 'OMG PONIES!!!11 :-D',
  'too_many_io_errors.ogg' => 'Too many I/O errors!',
  'long_live_http.ogg'     => 'Long live HTTP!',
  'connection_broken.ogg'  => 'Connection broken!',
  'i_hope_you_catch_swine_flu.ogg' => 'I hope you catch swine flu!',
  'i_will_not_be_defeated.ogg'     => 'I will not be defeated by you!',
  'your_attack_is_weak.ogg'        => 'Your attack is weak!',
  'impossible.ogg'                 => 'I... impossible!',
  'ni.ogg'                         => 'Ni !!! Ni !!!',
  'never_gonna_give_you_up.ogg'    => 'Never gonna give you up!',
  'nooo.ogg'                       => 'NOOHHHhurrghhhaaaahhh ... urrghhah',
  'boom_headshot.ogg'              => 'BOOM ! HEADSHOT!'
}

class SoundPool
  @@sounds = {}
  
  def self.get(window, file_name)
    if @@sounds[file_name].nil?
      @@sounds[file_name] = Gosu::Sample.new(window, file_name)
    end
    
    return @@sounds[file_name]
  end
end

#require 'weapon'

module Damageable
  attr_accessor :health
  
  def dead?
    @health <= 0
  end
  
  def take_damage_from(player)
    @health -= 5
    #@health -= player.weapon.damage
  end
end

#require 'player'
#require 'weapon'

class Player
  include Damageable
  STEP_SIZE = 12
  ANGLE_SPEED = 6
  FOV = 60.0 # Field of View
  HALF_FOV = FOV / 2
  DISTANCE_TO_PROJECTION = (Config::WINDOW_WIDTH / 2) / Math.tan((FOV / 2) * Math::PI / 180)
  RAY_ANGLE_DELTA = (FOV / Config::WINDOW_WIDTH)
  
  
  attr_accessor :x
  attr_accessor :y
  attr_accessor :height
  attr_accessor :angle
  attr_accessor :health
  attr_accessor :weapon
  attr_accessor :window
  attr_accessor :score
  attr_accessor :max_health
  
  def initialize(window)
    @x = 0.0
    @y = 0.0
    @angle  = 0.0
    @health = 100
    @window = window
    @score  = 0
    @max_health = 100
  end
  
  def angle_in_radians
    @angle * Math::PI / 180
  end
  
  def turn_left
    @angle = (@angle + ANGLE_SPEED) % 360
  end
  
  def turn_right
    # The added 360 here will make sure that @angle >= 0
    @angle = (360 + @angle - ANGLE_SPEED) % 360
  end
  
  def dx
    # x = r cos(theta)
    STEP_SIZE * Math.cos(self.angle_in_radians)
  end
  
  def dy
    # y = r sin(theta)
    STEP_SIZE * Math.sin(self.angle_in_radians)
  end
  
  def can_move_forward?(map)
    return !map.hit?(@x + 4*dx, @y - 4*dy)
  end
  
  def can_move_backward?(map)
    return !map.hit?(@x - 4*dx, @y + 4*dy)
  end
  
  def move_forward
    @x += dx
    @y -= dy
  end
  
  def move_backward
    @x -= dx
    @y += dy
  end
  
  def health_percent
    @health * 100.0 / @max_health
  end

  def take_damage_from(player)
    return if @health <= 0
    @health -= 4 # TODO: @health -= player.weapon.damage
    @health = 0 if @health < 0
  end
  
end

#require 'ai_player'
#
#require 'map'
#require 'sprite'
#require 'weapon'
#require 'sound'

module AStar
  Coordinate = Struct.new(:x, :y)
  
  def find_path(map, start, goal)
    start  = Coordinate.new(start[0], start[1])
    goal   = Coordinate.new(goal[0], goal[1])
    
    closed = []
    open   = [start]
    
    g_score = {}
    h_score = {}
    f_score = {}
    came_from = {}
    
    g_score[start] = 0
    h_score[start] = heuristic_estimate_of_distance(start, goal)
    f_score[start] = h_score[start]
    
    while not open.empty?
      x = smallest_f_score(open, f_score)
      return reconstruct_path(came_from, goal) if x == goal
      
      open.delete(x)
      closed << x
      neighbor_nodes = neighbor_nodes(map, x)
      
      neighbor_nodes.each do |y|
        next if closed.include?(y) or not map.walkable?(y.y, y.x)
        
        tentative_g_score = g_score[x] + dist_between(x, y)
        tentative_is_better = false
        if not open.include?(y)
          open << y
          h_score[y] = heuristic_estimate_of_distance(y, goal)
          tentative_is_better = true
        elsif tentative_g_score < g_score[y]
          tentative_is_better = true
        end
        
        if tentative_is_better
          came_from[y] = x
          g_score[y] = tentative_g_score
          f_score[y] = g_score[y] + h_score[y]
        end
      end
    end
    
    # No path found
    return nil
  end
  
  def dist_between(a, b)
    col_a, row_a = Map.matrixify(a.x, a.y)
    col_b, row_b = Map.matrixify(b.x, b.y)
    
    if col_a == col_b && row_a != row_b
      1.0
    elsif col_a != col_b && row_a == row_b
      1.0
    else
      1.4142135623731 # Sqrt(1**2 + 1**2)
    end
  end
  
  def neighbor_nodes(map, node)
    node_x, node_y = node.x, node.y
    result = []

    x = node_x - 1
    x_max = node_x + 1
    y_max = node_y + 1
    while(x <= x_max && x < map.width)
      y = node_y - 1
      
      while(y <= y_max && y < map.height)
        result << Coordinate.new(x, y) unless (x == node_x && y == node_y)
        y += 1
      end
      
      x += 1
    end
    
    return result
    
  end
  
  def heuristic_estimate_of_distance(start, goal)
    # Manhattan distance
    (goal.x - start.x).abs + (goal.y - start.y).abs
  end
  
  def reconstruct_path(came_from, current_node)
    #puts "START TRACE"
    
    while came_from[current_node]
      #puts "#{current_node[0]}, #{current_node[1]}"
      parent = came_from[current_node]
      
      if came_from[parent].nil?
        # No more parent for this node, return the current_node
        return current_node
      else
        current_node = parent
      end
    end
    
    #puts "No path found"
  end
  
  def smallest_f_score(list_of_coordinates, f_score)
    x_min = list_of_coordinates[0]
    f_min = f_score[x_min]
    
    list_of_coordinates.each {|x|
      if f_score[x] < f_min
        f_min = f_score[x]
        x_min = x
      end
    }
    
    return x_min
  end
  
end

module Sprite
  TEX_WIDTH  = 64
  TEX_HEIGHT = 64
  
  attr_accessor :x
  attr_accessor :y
  attr_accessor :window
  attr_accessor :slices
  attr_accessor :z_order
end

class AIPlayer
  include AStar
  include Sprite
  include Damageable
  
  # Maximum distance (in blocks) that this player can see.
  attr_accessor :sight
  # This enemy must not be closer than the given number of blocks to the main character.
  attr_accessor :min_dinstance
  # Whether the AI for this sprite is active.
  attr_accessor :active
  
  attr_accessor :steps_removed_from_player
  
  def initialize(sight = 10, min_distance = 2)
    @sight = sight
    @min_distance = min_distance
    @active = true
  end
  
  def interact(player, drawn_sprite_x)
    return if @health <= 0 || !@active
    
    self.current_state = :idle if @current_state == :firing && @firing_left == 0
    
    if @firing_left > 0
      if (@current_anim_seq_id == 0)
        self.fire(player)
      end
      @firing_left -= 1
      return
    end
    
    if (drawn_sprite_x.include?(self) && rand > 0.8)
      @firing_left = 1 + rand(5)
    end
    
    #dx = player.x - @x
    #dy = (player.y - @y) * -1
    
    #angle_rad = Math::atan2(dy, dx)
    #dx = @steps_removed_from_player * @step_size * Math::cos(angle_rad)
    #dy = @steps_removed_from_player * @step_size * Math::sin(angle_rad)
    
    dx = 0
    dy = 0
    
    start = Coordinate.new(*Map.matrixify(@x, @y))
    goal  = Coordinate.new(*Map.matrixify(player.x - dx, player.y - dy))
    if heuristic_estimate_of_distance(start, goal) > @min_distance
      path  = self.find_path(@map, start, goal)
      if path
        self.step_to_adjacent_squarily(path.y, path.x)
      end
    end
  end
end

class Enemy < AIPlayer
  FIRING_SOUND_BLOCKS = 2.5
  
  attr_accessor :step_size
  attr_accessor :animation_interval
  
  def initialize(window, kind_tex_paths, map, x, y, death_sound, firing_sound, kill_score = 100, step_size = 4, animation_interval = 0.2)
    super()
    @window = window
    @x = x
    @y = y
    @slices = {}
    @health ||= 100
    @map = map
    @steps_removed_from_player = 22
    @firing_left = 0
    @kill_score  = kill_score
    @firing_sounds = load_sounds(firing_sound)
    @death_sounds  = load_sounds(death_sound)
    @name       ||= self.class.to_s
    #@firing_text  = "#{@name}: \"#{SOUND_TO_TEXT[firing_sound]}\"" if SOUND_TO_TEXT.has_key?(firing_sound)
    #@death_text   = "#{@name}: \"#{SOUND_TO_TEXT[death_sound]}\"" if SOUND_TO_TEXT.has_key?(death_sound)
    
    kind_tex_paths.each { |kind, tex_paths|
      @slices[kind] = []
      tex_paths.each { |tex_path|
        @slices[kind] << SpritePool::get(window, tex_path, TEX_HEIGHT)
      }
    }
    
    @step_size = step_size
    @animation_interval = animation_interval
    
    self.current_state = :idle
    @last_draw_time = Time.now.to_f
  end
  
  def take_damage_from(player)
    return if @current_state == :dead
    @health -= 5 # TODO: Need to refactor this to take into account different weapons.
    if @health > 0
      self.current_state = :damaged
    else
      self.current_state = :dead
      @firing_sound_sample.stop if @firing_sound_sample
      play_random_sound(@death_sounds)
      player.score += @kill_score
    end
  end
  
  def step_to_adjacent_squarily(target_row, target_column)
    my_column, my_row = Map.matrixify(@x, @y)
    x = my_column
    y = my_row
    
    if my_column == target_column || my_row == target_row
      type = "orthogonal"
      # Orthogonal
      x = target_column # * Map::GRID_WIDTH_HEIGHT
      y = target_row    # * Map::GRID_WIDTH_HEIGHT
    else
      # Diagonal
      type = "diagonal"
      x = my_column
      y = target_row
      
      if not @map.walkable?(y, x)
        x = target_column
        y = my_row
      end
    end
    
    x += 0.5
    y += 0.5
    
    x *= Map::GRID_WIDTH_HEIGHT
    y *= Map::GRID_WIDTH_HEIGHT
    
    #puts "#{Time.now} -- (#{x}, #{y})"
    self.step_to(x, y)
    
  end
  
  def step_to(x, y)
    return if @current_state == :dead
    
    if (@x == x && @y == y)
      self.current_state = :idle
      return
    end
    
    self.current_state = :walking if self.current_state != :walking &&
      @current_anim_seq_id + 1 == @slices[@current_state].size
    
    dx = x - @x
    dy = (y - @y) * -1
    
    angle_rad = Math::atan2(dy, dx) * -1
    
    @x += @step_size * Math::cos(angle_rad)
    @y += @step_size * Math::sin(angle_rad)
  end
  
  def current_state
    @current_state
  end
  
  def current_state=(state)
    @current_state       = state
    @current_anim_seq_id = 0
    if state == :idle || state == :walking || state == :firing
      @repeating_anim = true
    else
      @repeating_anim = false
    end
  end
  
  def slices
    # Serve up current slice
    now = Time.now.to_f
    
    if @current_state == :dead && @current_anim_seq_id + 1 == @slices[:dead].size && !@on_death_called
      @on_death_called = true
      on_death if respond_to?(:on_death, true)
    end
    
    if not (( @current_state == :dead and @current_anim_seq_id + 1 == @slices[:dead].size ) or (@current_state == :idle))
      if now >= @last_draw_time + @animation_interval
        @current_anim_seq_id += 1
        if @repeating_anim
          @current_anim_seq_id = @current_anim_seq_id % @slices[@current_state].size
        else
          if @current_anim_seq_id >= @slices[@current_state].size
            self.current_state = :idle
          end
        end
        
        @last_draw_time = now
      end
    end
    
    return @slices[@current_state][@current_anim_seq_id]
  end
  
  def fire(player)
    return if @current_status == :dead
    dx = player.x - @x
    dy = player.y - @y
    r_2 = dx * dx + dy * dy
    f_2 = FIRING_SOUND_BLOCKS * FIRING_SOUND_BLOCKS * Map::GRID_WIDTH_HEIGHT * Map::GRID_WIDTH_HEIGHT
    r_2 = f_2 if r_2 < f_2
    
    volume = f_2 / (r_2 * 1.25)
    
    if @firing_sound_sample.nil? || !@firing_sound_sample.playing?
      @firing_sound_sample = play_random_sound(@firing_sounds)
    end
    player.take_damage_from(self)
    
    self.current_state = :firing
  end
  
  private
  
  def load_sounds(sounds)
    sounds = [sounds] if !sounds.is_a?(Array)
    sounds.map do |sound_file|
      { :file => sound_file, :sound => SoundPool.get(@window, sound_file) }
    end
  end
  
  def play_random_sound(sounds)
    sound = sounds[rand(sounds.size)]
    text = SOUND_TO_TEXT[sound[:file]]
    @window.show_text("#{@name}: \"#{text}\"") if text
    sound[:sound].play
  end
end

class MeleeEnemy < Enemy
  def interact(player, drawn_sprite_x)
    return if @health <= 0
    
    self.current_state = :idle if @current_state == :firing && @firing_left == 0
    
    if @firing_left > 0
      if (@current_anim_seq_id == 0)
        self.fire(player)
      end
      @firing_left -= 1
      return
    end
    
    start = Coordinate.new(*Map.matrixify(@x, @y))
    goal  = Coordinate.new(*Map.matrixify(player.x, player.y))
    
    h = heuristic_estimate_of_distance(start, goal)
    
    if h > @min_distance
      path  = self.find_path(@map, start, goal)
      if path
        self.step_to_adjacent_squarily(path.y, path.x)
      end
    elsif h == @min_distance && drawn_sprite_x.include?(self) && rand > 0.5
      @firing_left = 1 + rand(5)
    end
  end
end

class Guard < Enemy
  def initialize(window, map, x, y, death_sound = nil, firing_sound = nil, kill_score = 100, step_size = 3, animation_interval = 0.2)
    sprites = {
      :idle    => ['guard_idle.png'],
      :walking => ['guard_walking.png', 'guard_walking2.png', 'guard_walking3.png', 'guard_walking4.png'],
      :firing  => ['guard_firing.png', 'guard_firing2.png'],
      :damaged => ['guard_damaged.png', 'guard_dead.png'],
      :dead    => ['guard_dead.png', 'guard_dead2.png', 'guard_dead3.png', 'guard_dead4.png', 'guard_dead5.png']
    }
    
    sounds  = ['long live php.ogg', 'myphplife.ogg', 'my damn php life.ogg', 'phpforever.ogg']
    firing_sound ||= sounds[rand(sounds.size - 1)]
    death_sound  ||= sounds[rand(sounds.size - 1)]
    
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
    @health = 50
  end
end

class Hans < Enemy
  def initialize(window, map, x, y, death_sound = nil, firing_sound = 'machine_gun_burst.ogg', kill_score = 1000, step_size = 3, animation_interval = 0.2)
    sprites = {
      :idle    => ['hans1.bmp'],
      :walking => ['hans1.bmp', 'hans2.bmp', 'hans3.bmp', 'hans4.bmp'],
      :firing  => ['hans5.bmp', 'hans6.bmp', 'hans7.bmp'],
      :damaged => ['hans8.bmp', 'hans9.bmp'],
      :dead    => ['hans9.bmp', 'hans10.bmp', 'hans11.bmp']
    }
    
    # Special thanks goes out to Julian Raschke (jlnr on #gosu@irc.freenode.net ) of libgosu.org for recording these samples for us.
    death_sounds  = ['mein_spagetthicode.ogg', 'meine_magischen_qpc.ogg', 'meine_sql.ogg', 'meine_sql.ogg']
    death_sound ||= death_sounds[rand(death_sounds.size - 1)]
    
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
  end
end

class Ronald < Enemy
  def initialize(window, map, x, y, death_sound = 'balloon.ogg', firing_sound = 'floating.ogg', kill_score = 2000, step_size = 3, animation_interval = 0.2)
    sprites = {
      :idle    => ['ronald.png'],
      :walking => ['ronald_moving.png', 'ronald_moving2.png'],
      :firing  => ['ronald_attack.png', 'ronald_attack2.png'],
      :damaged => ['ronald_damaged.png'],
      :dead    => ['ronald_dead.png', 'ronald_dead2.png', 'ronald_dead3.png', 'ronald_dead4.png',
                   'ronald_dead5.png', 'ronald_dead6.png', 'ronald_dead7.png', 'ronald_dead8.png',
                   'ronald_dead9.png', 'ronald_dead10.png']
    }
    
    @name = "Pennywise McDonalds"
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
    @health = 250
  end
  
  def on_death
    @map.players.delete(self)
    @map.items << Fries.new(@window, @map, x, y)
  end
end

class Hongli < Enemy
  def initialize(window, map, x, y, death_sound = nil, firing_sound = nil, kill_score = 10000, step_size = 3, animation_interval = 0.2, &on_death)
    sprites = {
      :idle    => ['hongli.png'],
      :walking => ['hongli.png'],
      :firing  => ['hongli_attack.png', 'hongli_attack2.png'],
      :damaged => ['hongli_damaged.png'],
      :dead    => ['hongli_dead.png', 'hongli_dead2.png', 'hongli_dead3.png', 'hongli_dead4.png']
    }
    
    death_sound  ||= 'impossible.ogg'
    firing_sound ||= ['i_hope_you_catch_swine_flu.ogg', 'i_will_not_be_defeated.ogg', 'your_attack_is_weak.ogg']
    
    @name = "Hongli Lai"
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
    @health = 350
    @on_death = on_death
  end
  
  private
  
  def on_death
    @on_death.call if @on_death
  end
end

class Ninh < Enemy
  def initialize(window, map, x, y, death_sound = nil, firing_sound = nil, kill_score = 10000, step_size = 3, animation_interval = 0.2, &on_death)
    sprites = {
      :idle    => ['ninh.png'],
      :walking => ['ninh.png'],
      :firing  => ['ninh_attack.png'],
      :damaged => ['ninh_damaged.png'],
      :dead    => ['ninh_dead.png', 'ninh_dead2.png', 'ninh_dead3.png', 'ninh_dead4.png']
    }
    
    death_sound  ||= 'nooo.ogg'
    firing_sound ||= ['never_gonna_give_you_up.ogg', 'ni.ogg', 'boom_headshot.ogg']
    
    @name = "Ninh Bui"
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
    @health = 350
    @on_death = on_death
  end
  
  private
  
  def on_death
    @on_death.call if @on_death
  end
end

class Zed < Enemy
  def initialize(window, map, x, y, death_sound = 'omgponies.ogg', firing_sound = ['test_all_the_effing_time_is_lame.ogg', 'guitar_weapon.ogg', 'guitar_weapon2.ogg'], kill_score = 10000, step_size = 3, animation_interval = 0.2)
    sprites = {
      :idle    => ['rockzed.png'],
      :walking => ['rockzed_moving.png', 'rockzed_moving2.png'],
      :firing  => ['rockzed_attacking.png', 'rockzed_attacking2.png', 'rockzed_attacking3.png',
                   'rockzed_attacking4.png', 'rockzed_attacking5.png', 'rockzed_attacking6.png',
                   'rockzed_attacking7.png', 'rockzed_attacking8.png', 'rockzed_attacking9.png'],
      :damaged => ['rockzed_damaged.png'],
      :dead    => ['magic_pony.png']
    }
    
    @name = "Zed Shaw"
    @health = 1337 # That way we can hear the nice evil sound sample ;-)
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
  end
end

class Thin < Enemy
  def initialize(window, map, x, y, death_sound = nil, firing_sound = nil, kill_score = 500, step_size = 3, animation_interval = 0.5)
    sprites = {
      :idle    => ['thin.png'],
      :walking => ['thin.png', 'thin2.png'],
      :firing  => ['thin_attacking.png', 'thin_attacking2.png'],
      :damaged => ['thin_damaged.png'],
      :dead    => ['thin_dead.png', 'thin_dead2.png', 'thin_dead3.png', 'thin_dead4.png']
    }
    
    sounds = ['connection_broken.ogg', 'long_live_http.ogg', 'too_many_io_errors.ogg']
    death_sound  ||= sounds
    firing_sound ||= sounds
    
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
    @health = 200
    @min_distance = 1
  end
end

class Dog < MeleeEnemy
  def initialize(window, map, x, y, death_sound = 'dog_cry.ogg', firing_sound = 'dog_bark.ogg', kill_score = 500, step_size = 7, animation_interval = 0.2)
    sprites = {
      :idle => ['dog_walking.png'],
      :walking => ['dog_walking.png', 'dog_walking2.png', 'dog_walking3.png', 'dog_walking4.png'],
      :firing  => ['dog_attacking.png', 'dog_attacking2.png', 'dog_attacking3.png'],
      :damaged => ['dog_dead.png', 'dog_dead2.png'],
      :dead    => ['dog_dead.png', 'dog_dead2.png', 'dog_dead3.png', 'dog_dead4.png']
    }
    
    @name = "Mongrel"
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
    @health = 100
    @min_distance = 1
  end
end

class DavidHasslehoff < MeleeEnemy
  def initialize(window, map, x, y, death_sound = 'dog_cry.ogg', firing_sound = 'machine_gun_burst.ogg', kill_score = 500, step_size = 7, animation_interval = 0.2)
    sprites = {
      :idle => ['david_hasselhoff.png'],
      :walking => ['david_hasselhoff.png'],
      :firing  => ['david_hasselhoff_attack.png'],
      :damaged => ['david_hasselhoff_damaged.png'],
      :dead    => ['david_hasselhoff_damaged.png', 'david_hasselhoff_dead2.png', 'david_hasselhoff_dead3.png', 'david_hasselhoff_dead4.png']
    }
    
    @name = "David Hasslehoff"
    super(window, sprites, map, x, y, death_sound, firing_sound, kill_score, step_size, animation_interval)
    @health = 250
  end
end



#require 'sprite'
#
#require 'rubygems'
#require 'gosu'
#require 'sound'
#require 'weapon'
#require 'map'



class SpritePool
  @@files = {}
  
  def self.get(window, file_path, sprite_height = Sprite::TEX_HEIGHT, sprite_width = 1)
    file_path = File.expand_path(file_path)
    if !@@files[file_path]
      begin
        @@files[file_path] = Gosu::Image::load_tiles(window, file_path, sprite_width, sprite_height, true)
      rescue
        STDERR.puts "Cannot load #{file_path}"
        raise
      end
    end
    
    return @@files[file_path]
  end
end

class MagicPony
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'magic_pony.png', TEX_HEIGHT)
  end
end

class Lamp
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'lamp.bmp', TEX_HEIGHT)
  end
end

class Table
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'table.png', TEX_HEIGHT)
  end
end

class TableWithChairs
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'tablechairs.png', TEX_HEIGHT)
  end
end

class Well
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'well.png', TEX_HEIGHT)
  end
end

class BrownBarrel
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'brownbarrel.png', TEX_HEIGHT)
  end
end

class Chandelier
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'chandelier.bmp', TEX_HEIGHT)
  end
end

class DeadGuard
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'deadguard.bmp', TEX_HEIGHT)
  end
end

class Bones
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'bones.bmp', TEX_HEIGHT)
  end
end

class Skeleton
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'skeleton.bmp', TEX_HEIGHT)
  end
end

class GreenPlant
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'plantgreen.png', TEX_HEIGHT)
  end
end

class Flag
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'flag.png', TEX_HEIGHT)
  end
end

class Armor
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'armor.png', TEX_HEIGHT)
  end
end

class Vase
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'vase.png', TEX_HEIGHT)
  end
end

class ColonelSanders
  include Sprite
  
  def initialize(window, x, y)
    @window = window
    @x = x
    @y = y
    @slices = SpritePool::get(window, 'colonel_sanders.bmp', TEX_HEIGHT)
  end
end


# An sprite that interacts with the player when they touch each other.
class Interactable
  include Sprite
  
  attr_accessor :window
  attr_accessor :x
  attr_accessor :y
  attr_accessor :map
  
  def initialize(window, map, x, y, slices)
    @window = window
    @x = x
    @y = y
    @map = map
    @slices = slices
    @last_interaction_time = 0
  end
  
  def interact(player)
    if interaction_has_effect?(player)
      @last_interaction_time = Time.now.to_f
      execute_interaction_effect(player)
    end
  end

  private
  
  def interaction_has_effect?(player)
    # Hack: do not repeatedly interact with player who's standing still.
    if @last_interaction_time + 10 < Time.now.to_f
      my_row, my_column = Map.matrixify(@y, @x)
      player_row, player_column = Map.matrixify(player.y, player.x)
      my_row == player_row && my_column == player_column
    else
      false
    end
  end
  
  def execute_interaction_effect(player)
  end
end

# An item that can be picked up once.
class Item < Interactable
  def initialize(window, map, x, y, slices, text = nil, sound_file = nil)
    super(window, map, x, y, slices)
    @text  = text
    @sound = SoundPool::get(window, sound_file || 'ammo.ogg')
  end
  
  private
  
  def execute_interaction_effect(player)
    super(player)
    @sound.play
    @window.show_text(@text) if @text
    @map.items.delete(self)
  end
end

# Item which increases or decreases HP. Will only interact when the player's
# HP is < 100% (for positive powerups) and > 0% (for negative power ups).
class Powerup < Item
  def initialize(window, map, x, y, slices, power_up, text = nil, sound_file = nil)
    super(window, map, x, y, slices, text, sound_file)
    @power_up = power_up
  end
  
  private
  
  def interaction_has_effect?(player)
    super(player) && (
      (@power_up > 0 && player.health < player.max_health) ||
      (@power_up < 0 && player.health > 0)
    )
  end
  
  def execute_interaction_effect(player)
    super(player)
    new_health = player.health + @power_up
    if new_health > player.max_health
      new_health = player.max_health
    elsif new_health < 0
      new_health = 0
    end
    player.health = new_health
  end
end

class Food < Powerup
  def initialize(window, map, x, y)
    super(window, map, x, y, SpritePool::get(window, 'food.bmp', TEX_HEIGHT), 25, "Food: +25 HP!")
  end
end

class Peepcode < Powerup
  def initialize(window, map, x, y)
    super(window, map, x, y, SpritePool::get(window, 'peepcode_powerup.png', TEX_HEIGHT), 50, "Peepcode: +50 HP!")
  end
end

class Rails < Powerup
  def initialize(window, map, x, y)
    super(window, map, x, y, SpritePool::get(window, 'rails.bmp', TEX_HEIGHT), 100, "Rails: +100 HP!")
  end
end

class PHP < Powerup
  def initialize(window, map, x, y)
    super(window, map, x, y, SpritePool::get(window, 'php.png', TEX_HEIGHT), -25,
          'PHP: "Fuck you!"', 'fuck_you.ogg')
  end
end

class Fries < Powerup
  def initialize(window, map, x, y)
    super(window, map, x, y, SpritePool::get(window, 'ronald_dead10.png', TEX_HEIGHT), 40,
          "French Fries: +40 HP!\nBut don't eat too much, it's bad for your health.",
          'fuck_you.ogg')
  end
end

class Info < Interactable
  attr_accessor :play_sound
  
  def initialize(window, map, x, y, text = nil, change_bg_song_to = nil, &block)
    super(window, map, x, y, SpritePool::get(window, @image || 'info.png', TEX_HEIGHT))
    @play_sound = true
    @text = text
    @sound = SoundPool::get(window, 'Message_Received.ogg')
    @change_bg_song_to = change_bg_song_to
    @block = block
  end
  
  private
  
  def execute_interaction_effect(player)
    super(player)
    @window.show_text(@text) if @text
    @window.background_song = @change_bg_song_to if @change_bg_song_to
    @block.call(self, player) if @block
    @sound.play if @play_sound
  end
end

class InvisibleInfo < Info
  def initialize(window, map, x, y, text = nil, change_bg_song_to = nil, &block)
    @image = 'invisible_item.png'
    super(window, map, x, y, text, change_bg_song_to, &block)
  end
end

class Phusion < Item
  def initialize(window, map, x, y, new_max_health)
    super(window, map, x, y, SpritePool::get(window, 'phusion_logo.png', TEX_HEIGHT))
    @new_max_health = new_max_health
  end
  
  private
  
  def execute_interaction_effect(player)
    super(player)
    @window.show_text("Phusion: Max HP increased from #{player.max_health} to #{@new_max_health}!")
    player.max_health = @new_max_health
    player.health = @new_max_health
  end
end


#require 'door'

#require 'map'

class Door
  attr_accessor :pos
  attr_reader   :state
  attr_reader   :opened_at
  OPEN_CLOSE_STEP = 8
  STAYS_SECONDS_OPEN = 4
  FULL_VOLUME_WITHIN_GRID_BLOCKS = 5.0

  def initialize
    @state = :closed
    @pos   = 0
    @opened_at = 0
  end

  def open!
    if self.closed?
      @state = :opening
      @opened_at = Time.now.to_i
    end
    
    if !self.open? && @state == :opening
      @pos += OPEN_CLOSE_STEP
    end
  end
  
  def open?
    return @pos == Map::GRID_WIDTH_HEIGHT
  end
  
  def close!
    if self.open?
      @state = :closing
    end
    
    if !self.closed? && @state == :closing
      @pos -= OPEN_CLOSE_STEP
    end
  end
  
  def closed?
    return @pos == 0
  end
  
  def interact
    if @state == :opening
      self.open!
    elsif @state == :closing
      self.close!
    end
  end
  
  def inspect
    if open?
      "#<Door:#{object_id} open>"
    else
      "#<Door:#{object_id} open>"
    end
  end
  
end

#require 'image_pool'

#require 'rubygems'
#require 'gosu'

class ImagePool
  @@image_files = {}
  @@image_texts = {}
  
  def self.get_file(window, filename)
    if !@@image_files.has_key?(filename)
      @@image_files[filename] = Gosu::Image.new(window, filename)
    end
    @@image_files[filename]
  end
  
  def self.get_text(window, text)
    if !@@image_texts.has_key?(text)
      lines = text.split("\n")
      @@image_texts[text] = lines.map do |line|
        Gosu::Image.from_text(window, line, "Arial", 26)
      end
    end
    @@image_texts[text]
  end
end

#require 'level'
#require 'map'

class Level0
private
  def new
  end
  
public
  MATRIX = [
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,18,5,5,5,5,5,18,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,0,0,0,0,0,0,0,0,0,0,0,5,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,0,0,0,0,4,1,1,1,1,1,1,4,4,4,4,4,4,4,13,4,4,4,18,0,0,0,0,0,0,0,0,0,0,0,18,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,0,0,0,0,4,4,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,0,0,0,0,0,4,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,0,0,0,0,0,4,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,4,24,4,4,25,4,4,24,4,4,1,4,0,0,0,4,4,4,13,4,4,4,14,0,0,0,0,0,0,0,0,0,0,0,14,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,4,1,4,0,0,0,4,1,1,1,1,1,1,5,0,0,0,0,0,0,0,0,0,0,0,5,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,13,0,0,0,0,0,0,0,0,0,13,4,4,0,0,0,4,4,4,1,1,1,1,5,5,5,18,5,21,-1,7,5,18,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,4,1,1,1,1,5,5,5,5,5,0,0,0,5,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,4,1,1,1,1,1,1,5,5,5,0,0,0,5,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,4,1,1,1,1,1,1,5,5,14,0,0,0,14,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,4,4,4,4,13,0,0,0,0,0,0,0,0,0,13,4,4,4,24,4,4,4,1,1,1,1,1,1,1,5,5,5,0,0,0,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,4,0,0,0,0,0,0,0,0,0,4,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,0,0,0,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,4,4,24,4,9,-1,9,4,24,4,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,0,0,0,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,4,4,4,4,0,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,1,5,5,0,0,5,0,0,0,0,0,0,0,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,9,-1,9,4,4,4,4,4,0,0,0,4,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,0,0,5,5,5,0,0,0,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,4,4,4,4,0,0,0,4,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,0,0,5,5,5,0,0,0,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,0,0,0,4,0,0,0,4,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,0,0,5,5,5,0,0,0,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,0,0,0,0,4,0,0,0,4,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,18,0,0,0,18,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,0,0,0,4,0,0,0,4,4,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,5,5,0,0,0,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,4,4,4,4,9,-1,9,4,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,7,-1,7,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,4,0,0,0,0,0,0,0,0,0,4,4,4,1,1,1,1,1,1,1,1,4,4,4,4,4,14,0,0,0,0,0,14,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,4,4,0,0,0,0,0,0,0,0,0,4,4,4,1,1,1,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,4,4,0,0,0,0,0,0,0,0,0,4,4,4,1,1,1,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,0,8,0,0,0,0,0,0,0,0,0,4,4,1,1,1,1,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,4,4,1,1,1,1,1,1,1,1,1,4,0,0,0,0,15,16,0,0,0,17,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,0,8,0,0,0,0,0,0,0,0,0,4,4,1,1,1,1,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,4,4,0,0,0,0,0,0,0,0,0,4,4,1,1,1,1,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,4,4,0,0,0,0,0,0,0,0,0,4,4,1,1,1,1,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,1,1,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,4,4,0,0,0,0,0,0,0,0,0,4,4,1,1,1,1,1,1,1,1,1,4,4,4,4,4,14,0,0,0,0,0,14,4,4,4,4,4,1,1,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1],
  	[4,0,0,0,4,4,4,4,4,9,-1,9,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,4,22,-1,4,4,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1],
  	[4,0,0,0,4,4,4,4,4,0,0,0,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,4,4,4,4,0,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,4,0,0,0,4,0,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,0,0,0,0,4,0,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,1,1],
  	[4,0,0,0,4,0,0,0,4,0,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,2,0,0,0,0,0,0,0,0,0,1,1,1,1],
  	[4,0,0,0,4,4,4,4,4,0,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,-1,0,0,0,0,0,0,0,0,0,1,1,1,1],
  	[4,0,0,0,4,4,4,4,4,0,0,0,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,2,0,0,0,0,0,0,0,0,0,1,1,1,1],
  	[4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,8,4,4,4,4,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,1,1,1,1],
  	[4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,-1,0,4,4,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,8,4,4,4,4,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,4,4,8,0,0,4,0,0,4,0,0,4,1,1,1,1,1,1,1,1,1,1,1,1,3,-1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,4,0,-1,0,0,1,0,0,4,0,4,1,1,1,1,1,1,1,1,0,0,0,0,2,0,0,0,2,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,4,4,8,0,0,0,0,0,4,4,4,1,1,1,1,1,1,1,1,0,0,0,0,-1,0,0,0,-1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,0,0,0,0,2,0,0,0,2,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,4,4,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,0,0,20,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,4,4,0,0,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,19,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,2,0,0,0,2,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,-1,0,0,0,-1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,2,0,0,0,2,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  	[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,11,1,1,23,1,1,12,1,1,11,1,1,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  ]

  WORLD_TEXTURES = [
    { :north => 'blue1_1.png', :east => 'blue1_2.png', :south => 'blue1_1.png', :west => 'blue1_2.png' },
    { :north => 'door_s_1.png', :east => 'blue1_2.png', :south => 'door_s_1.png', :west => 'blue1_2.png' },
    { :north => 'blue1_1.png', :east => 'door_s_2.png', :south => 'blue1_1.png', :west => 'door_s_2.png' },
    
    { :north => 'grey1_1.png', :east => 'grey1_2.png', :south => 'grey1_1.png', :west => 'grey1_2.png' },
    
    { :north => 'wood1_1.png', :east => 'wood1_2.png', :south => 'wood1_1.png', :west => 'wood1_2.png' },
    { :north => 'door_s_1.png', :east => 'wood1_2.png', :south => 'door_s_1.png', :west => 'wood1_2.png' },
    { :north => 'wood1_1.png', :east => 'door_s_2.png', :south => 'wood1_1.png', :west => 'door_s_2.png' },
    
    # 8
    { :north => 'door_s_1.png', :east => 'grey1_2.png', :south => 'door_s_1.png', :west => 'grey1_2.png' },
    { :north => 'grey1_1.png', :east => 'door_s_2.png', :south => 'grey1_1.png', :west => 'door_s_2.png' },
    
    # 10
    { :north => 'wood_php_1.png', :east => 'wood_php_1.png', :south => 'wood_php_1.png', :west => 'wood_php_1.png' },
    { :north => 'blue2_1.png', :east => 'blue1_2.png', :south => 'blue1_1.png', :west => 'blue1_2.png' },
    { :north => 'blue3_1.png', :east => 'blue3_2.png', :south => 'blue3_1.png', :west => 'blue3_2.png' },
    { :north => 'wall_gray_flag.png', :east => 'wall_gray_flag.png', :south => 'wall_gray_flag.png', :west => 'wall_gray_flag.png' },
    { :north => 'wall_wood_hitler.png', :east => 'wall_wood_hitler.png', :south => 'wall_wood_hitler.png', :west => 'wall_wood_hitler.png' },
    
    # 15
    { :north => 'pratik.png', :east => 'pratik.png', :south => 'pratik.png', :west => 'pratik.png' },
    { :north => 'koz.png', :east => 'koz.png', :south => 'koz.png', :west => 'koz.png' },
    { :north => 'yoda.png', :east => 'yoda.png', :south => 'yoda.png', :west => 'yoda.png' },
    
    # 18
    { :north => 'wall_wood_eagle.png', :east => 'wall_wood_eagle.png', :south => 'wall_wood_eagle.png', :west => 'wall_wood_eagle.png' },
    { :north => 'peter_cooper.png', :east => 'peter_cooper.png', :south => 'peter_cooper.png', :west => 'peter_cooper.png' },
    { :north => 'rubyinside.png', :east => 'rubyinside.png', :south => 'rubyinside.png', :west => 'rubyinside.png' },
    { :north => 'joshpeek.png', :east => 'door_s_2.png', :south => 'joshpeek.png', :west => 'door_s_2.png' },
    { :north => 'grey1_1.png', :east => 'grey1_2.png', :south => 'peepcode.png', :west => 'grey1_2.png' },
    { :north => 'antonio.png', :east => 'blue1_2.png', :south => 'antonio.png', :west => 'blue1_2.png' },
    
    # 24
    { :north => 'wall_gray_eagle.png', :east => 'wall_gray_eagle.png', :south => 'wall_gray_eagle.png', :west => 'wall_gray_eagle.png' },
    { :north => 'wall_gray_hitler.png', :east => 'wall_gray_hitler.png', :south => 'wall_gray_hitler.png', :west => 'wall_gray_hitler.png' },
    
    { :north => 'door_1.png',  :east => 'door_2.png',  :south => 'door_1.png',  :west => 'door_2.png' },
  ]

  if ARGV[0]
    PLAYER_X = ARGV[0].to_i * Map::GRID_WIDTH_HEIGHT
  else
    PLAYER_X = 29.5 * Map::GRID_WIDTH_HEIGHT
  end
  if ARGV[1]
    PLAYER_Y = ARGV[1].to_i * Map::GRID_WIDTH_HEIGHT
  else
    PLAYER_Y = 57.5 * Map::GRID_WIDTH_HEIGHT
  end
  PLAYER_ANGLE = 0
  
  def self.create(window)
    map = Map.new(MATRIX, WORLD_TEXTURES, PLAYER_X, PLAYER_Y, PLAYER_ANGLE, window)
    
    # Preload MagicPony so that it loads quickly next time.
    MagicPony.new(window, 0, 0)
    
    ####### Props #######
    
    # Southern (starting) room.
    map.add do |add|
      add.prop(DeadGuard, 31.5, 57.5)
      add.prop(Bones, 40.5, 57.5)
      add.prop(Bones, 28.5, 51.5)
      add.prop(Skeleton, 38.5, 52.5)
      add.prop(Skeleton, 35.5, 33.5)
      add.prop(Lamp, 31, 61.5)
      add.prop(Lamp, 34.5, 61.5)
      add.prop(Lamp, 38.5, 61.5)
      add.prop(Lamp, 34.5, 53.5)
      add.prop(Lamp, 34.5, 58.5)
      add.prop(Lamp, 34.5, 47.0)
      add.prop(Lamp, 34.5, 42.0)
      
      # Central room.
      add.prop(Chandelier, 39.0, 33.0)
      add.prop(Chandelier, 34.0, 33.0)
      add.prop(Chandelier, 29.0, 33.0)
      add.prop(GreenPlant, 27.5, 36.5)
      add.prop(GreenPlant, 27.5, 30.5)
      add.prop(GreenPlant, 41.5, 30.5)
      add.prop(GreenPlant, 41.5, 36.5)
      add.prop(Flag, 35.5, 29.5)
      add.prop(Flag, 33.5, 29.5)
      
      # Eastern room.
      add.prop(Table, 56.5, 35.5)
      add.prop(Table, 59.5, 35.5)
      add.prop(Well, 56.5, 38.5)
      add.prop(Well, 59.5, 38.5)
      add.prop(BrownBarrel, 61.5, 30.5)
      add.prop(BrownBarrel, 61.5, 29.5)
      add.prop(BrownBarrel, 60.5, 29.5)
      add.prop(BrownBarrel, 59.5, 29.5)
      
      add.prop(ColonelSanders, 56.5, 44.5)
      add.prop(TableWithChairs, 55.5, 45.5)
      add.prop(TableWithChairs, 55.5, 43.5)
      add.prop(Lamp, 54.5, 44.5)
      add.prop(Armor, 52.5, 42.5)
      add.prop(Armor, 52.5, 46.5)
      add.prop(Armor, 54.5, 42.5)
      add.prop(Armor, 54.5, 46.5)
      
      # Path to northern room.
      add.prop(Lamp, 34.5, 25.0)
      add.prop(Lamp, 34.5, 22.0)
      add.prop(Lamp, 34.5, 19.0)
      add.prop(Armor, 37.0, 22.5)
      add.prop(Armor, 32.0, 22.5)
      
      # Northern room.
      add.prop(Lamp, 34.0, 12.0)
      add.prop(TableWithChairs, 36.0, 9.5)
      add.prop(TableWithChairs, 32.0, 9.5)
      add.prop(Vase, 29.5, 8.5)
      add.prop(Vase, 39.5, 8.5)
      
      add.prop(Lamp, 24.5, 11.5)
      add.prop(Lamp, 19.5, 11.5)
      add.prop(Lamp, 19.5, 17.5)
    end
    
    ####### Players #######
    
    zed, hongli, ninh = nil
    map.add do |add|
      # Southern (starting) room.
      add.player(Guard, 40.5, 61.5)
      add.player(Dog, 37.5, 57.5)
      add.player(Thin, 34.5, 41.5)
      
      # Room of horizontal scaling.
      add.player(Dog, 37.5, 52.5)
      add.player(Dog, 38.0, 52.0)
      add.player(Dog, 38.5, 52.5)
      add.player(Dog, 39.0, 52.5)
      add.player(Dog, 37.5, 53.5)
      add.player(Dog, 38.0, 52.0)
      add.player(Dog, 38.5, 53.5)
      add.player(Dog, 39.0, 53.5)
      
      # Central room with Pratik, Koz and Yoda.
      add.player(Hans, 31.0, 35.0)
      add.player(Hans, 38.0, 31.0)
      
      # Eastern room.
      add.player(Guard, 49.5, 33.5)
      add.player(Hans, 47.5, 40.5)
      add.player(Thin, 49.5, 44.5)
      add.player(Hans, 59.5, 31.5)
      
      # Path to north room.
      add.player(Hans, 32.0, 22.0)
      add.player(Guard, 34.5, 26.0)
      add.player(Hans, 37.0, 22.0)
      
      # North room.
      add.player(Ronald, 42.0, 11.0)
      add.player(Ronald, 43.0, 12.0)
      zed = add.player(Zed, 35.5, 9.0)
      zed.active = false
      
      add.player(Guard, 22.5, 11.0)
      add.player(Guard, 24.5, 12.0)
      add.player(Guard, 19.5, 15.0)
      add.player(Guard, 22.5, 17.5)
      add.player(Hans, 17.5, 17.0)
      add.player(Thin, 22.5, 18.0)
      
      hongli = add.player(Hongli, 9.0, 19.0)
      hongli.active = false
      ninh = add.player(Ninh, 10.0, 17.0)
      ninh.active = false
    end
    
    ####### Items #######
    
    map.add do |add|
      # Southern (starting) room.
      add.item(Food, 37.5, 62.5)
      add.item(Food, 29.0, 52.0)
      add.item(PHP,  32.0, 61.0)
      add.item(Food, 39.0, 52.0)
      add.item(Food, 39.0, 52.5)
      add.item(Info, 34.5, 62.2, "AkitaOnRails: \"OMG HELP MEE! DUN WANNA DIE!!11\"")
      add.item(Info, 31.5, 62.2, "Antonio Cangiano: \"Now on sale: Ruby on\nRails for Microsoft Developers\"")
      add.item(Info, 35.5, 55.5, "Peter Cooper: \"You are our last hope.\nShow the people that Rails CAN scale!\"")
      add.item(Info, 35.5, 52.5, "Room of horizontal scaling\nGoal: shutdown pack of Mongrels")
      add.item(Info, 34.5, 51.5, "Room of vertical scaling\nStarring: Thin web server\nWarning: more powerful than a single mongrel!")
      
      # Central room with Pratik, Koz and Yoda.
      add.item(Peepcode, 37.0, 34.5)
      add.item(Food, 32.5, 35.0)
      add.item(Info, 32.0, 32.0, "Don't worry Pratik, Koz, I'll free you guys in no time!") do
        SoundPool.get(window, "Free You Guy in No Time.ogg").play
      end
      add.item(InvisibleInfo, 34.5, 29.5) do |item, player|
        if player.max_health < 150
          window.show_text('Yoda: "Wait, young hero. Do not go in there yet!"')
        else
          window.show_text('Yoda: "Go, young hero. May the source be with you."')
        end
      end
      add.item(Info, 36.5, 32.5) do |item, player|
        if player.max_health < 150
          window.show_text("Yoda: \"Strong enough to defeat the enemy, you are not.\n" +
            "Train yourself in the room east of here, you must!\"")
        else
          window.show_text("Yoda: \"Trained well, you have, young hero!\"")
        end
      end
      
      add.item(Rails, 34.5, 18.5)
      add.item(PHP, 35.5, 18.5)
      add.item(Info, 33.5, 17.5, "Josh Peek: \"No, don't go in there! Inside is ZED SHAW!'\"")
      add.item(InvisibleInfo, 34.5, 16.5, nil, "enter_zed.ogg")
      add.item(InvisibleInfo, 34.5, 14.5) do |item, player|
        window.present_boss("Zed Shaw", "rockzed_large.png")
        map.items.delete(item)
        zed.active = true
      end
      
      # Eastern room.
      add.item(Peepcode, 51.5, 34.5)
      add.item(Peepcode, 58.5, 30.5)
      add.item(Food, 57.5, 30.5)
      add.item(Peepcode, 56.5, 34.5)
      add.item(Phusion, 59.5, 36.5, 150)
      
      add.item(Food, 55.5, 44.5)
      add.item(Food, 54.5, 43.5)
      add.item(Food, 55.0, 45.5)
      add.item(Food, 54.5, 45.5)
      add.item(Food, 55.0, 43.5)
      add.item(InvisibleInfo, 44.5, 33.5) do |item, player|
        if player.max_health >= 150 && !@power_150_felt
          @power_150_felt = true
          window.show_text("I can feel it... THE POWER!!!")
          SoundPool.get(window, "I can feel the Power.ogg").play
        end
        item.play_sound = false
      end
      add.item(InvisibleInfo, 52.5, 33.5) do |item, player|
        if !@eaten_at_kfc
          window.show_text("Fighting makes me hungry.")
          SoundPool.get(window, "Fighting Makes me Hungry.ogg").play
        end
        item.play_sound = !@eaten_at_kfc
      end
      add.item(InvisibleInfo, 49.9, 44.5) do |item, player|
        if !@eaten_at_kfc
          window.show_text("Is there anything to eat?") 
          SoundPool.get(window, "Is there Anything to Eat.ogg").play
        end
        item.play_sound = !@eaten_at_kfc
      end
      add.item(InvisibleInfo, 51.5, 44.5) do |item, player|
        @eaten_at_kfc = true
        item.play_sound = false
      end
      
      # Northern room.
      add.item(Food, 32.5, 13.0)
      add.item(Food, 35.5, 13.0)
      add.item(Food, 32.5, 14.5)
      add.item(Food, 32.5, 11.5)
      add.item(Food, 32.5, 14.5)
      add.item(Food, 35.5, 14.5)
      add.item(Food, 35.5, 11.5)
      add.item(Phusion, 30.0, 10.0, 200)
      add.item(InvisibleInfo, 29.5, 11.5,
        "My god, if even Zed is involved then who's\n" +
        "the mastermind behind all of this?",
        "getthem.ogg") do |item, player|
        SoundPool.get(window, "Zed Involved.ogg").play
      end
      
      # Path to western room.
      add.item(Peepcode, 19.5, 10.5)
      add.item(Rails, 20.5, 18.5)
      
      # Western room.
      add.item(Peepcode, 13.5, 19.5)
      add.item(Peepcode, 13.5, 15.5)
      add.item(Rails, 7.5, 15.5)
      add.item(Rails, 7.5, 19.5)
      add.item(InvisibleInfo, 14.5, 17.5) do |item, player|
        window.present_boss("Hongli Lai & Ninh Bui", "phusion_guys.png", "FINAL BOSSES") do
          window.background_song = "theme_song.ogg"
        end
        map.items.delete(item)
        hongli.active = true
        ninh.active = true
      end
    end
    
    map
  end
end


module ZOrder
  BACKGROUND = 0
  LEVEL      = 1
  SPRITES    = 2
  WEAPON     = 9999
  HUD        = 10000
  SCREEN_FLASH    = HUD + 3
  TEXT_BACKGROUND = HUD + 4
  TEXT            = HUD + 5
  FADE_OUT_OVERLAY = HUD + 6
end

class GameWindow < Gosu::Window
  Infinity = 1.0 / 0
  SCREEN_FLASH_MAX_ALPHA = 100
  SCREEN_FLASH_STEP      = 5
  POWERDOWN_SCREEN_FLASH_COLOR = Gosu::Color.new(SCREEN_FLASH_MAX_ALPHA, 255, 0, 0)
  POWERUP_SCREEN_FLASH_COLOR   = Gosu::Color.new(SCREEN_FLASH_MAX_ALPHA, 141, 198, 63)
  TEXT_BACKGROUND_COLOR        = Gosu::Color.new(160, 0, 0, 0)
  TEXT_BACKGROUND_PADDING      = 6
  TEXT_VERTICAL_SPACING        = 1
  MIN_TEXT_APPEARENCE_TIME     = 3
  FADE_OUT_OVERLAY_COLOR       = Gosu::Color.new(0, 0, 0, 0)
  BOSS_PRESENTATION_BACKGROUND_COLOR = Gosu::Color.new(255, 0, 61, 204)
  BOSS_PRESENTATION_CYAN_LINE_COLOR  = Gosu::Color.new(255, 0, 186, 251)
  BOSS_PRESENTATION_WHITE_LINE_COLOR = Gosu::Color.new(255, 255, 255, 255)
  BOSS_PRESENTATION_TITLE_FONT = "Myriad Pro"
  BOSS_PRESENTATION_TITLE_FONT_SIZE = 35
  BOSS_PRESENTATION_FONT       = "Myriad Pro"
  BOSS_PRESENTATION_FONT_SIZE  = 45
  
  TOP  = 0
  LEFT = 0
  RIGHT = Config::WINDOW_WIDTH - 1
  BOTTOM = Config::WINDOW_HEIGHT - 1
  
  def initialize
    super(Config::WINDOW_WIDTH, Config::WINDOW_HEIGHT, Config::FULLSCREEN, 1000.0 / Config::FPS)
    self.caption = 'Rubystein 3d by Phusion CS Company'
    
    @map = MapPool.get(self, 0)
    
    @player = Player.new(self)
    @player.height = 0.5
    @player.x = @map.player_x_init
    @player.y = @map.player_y_init
    @player.angle = @map.player_angle_init
    
    @wall_perp_distances   = [0]   #* Config::WINDOW_WIDTH
    @drawn_sprite_x        = [nil] #* Config::WINDOW_WIDTH
    
    @hud = Gosu::Image::new(self, 'hud.png', true)
    @hud_numbers = SpritePool.get(self, 'numbers.png', 32, 16)
    @weapon_idle = Gosu::Image::new(self, 'hand1.bmp', true)
    @weapon_fire = Gosu::Image::new(self, 'hand2.bmp', true)
    @floor_ceil  = Gosu::Image::new(self, 'floor_ceil.png', true)
    self.background_song = nil  # Play default background song.
    @fire_sound = Gosu::Sample.new(self, 'fire.ogg')
    @door_open_sound = Gosu::Sample.new(self, 'dooropen.ogg')
    @door_close_sound = Gosu::Sample.new(self, 'doorclose.ogg')
    
    # Screenflashing counters
    @powerup_screen_flash   = 0
    @powerdown_screen_flash = 0
    
    @hud_portret = SpritePool::get(self, 'sean_connery.png', 60, 60)
    
    @mode = :normal
    
    @ai_schedule_index = 0
    @last_row = nil
    @last_col = nil
  end
  
  def background_song=(filename)
    @bg_song.stop if @bg_song
    @bg_song = Gosu::Song.new(self, filename || 'getthem.ogg')
    @bg_song.volume = 0.25
    @bg_song.play(true)
  end

  def update
    case @mode
    when :normal, :fading_out
      update_fade_out_progress
      old_player_health = @player.health
      if @mode != :fading_out
        process_movement_input
        invoke_players
        invoke_items
        invoke_doors
      end
      determine_screen_flash(old_player_health)
      
      row, col = Map.matrixify(@player.y, @player.x)
      if @last_row != row || @last_col != col
        puts "#{col},#{row}"
        @last_row = row
        @last_col = col
      end
      
    when :presenting_boss
      update_boss_presentation_progress
      
    else
      abort "Invalid mode '#{@mode}'"
    end
  end
  
  def draw
    case @mode
    when :normal, :fading_out
      draw_scene
      draw_sprites
      draw_weapon
      draw_hud
      draw_screen_flash
      draw_text
      draw_fade_out_overlay
      
    when :presenting_boss
      draw_boss_presentation
      
    else
      abort "Invalid mode '#{@mode}'"
    end
  end
  
  def show_text(text)
    @active_text = text
    @active_text_timeout = 0.6 + (text.size * 0.15)
    @active_text_timeout = MIN_TEXT_APPEARENCE_TIME if @active_text_timeout < MIN_TEXT_APPEARENCE_TIME
    @active_text_timeout = Time.now + @active_text_timeout
  end
  
  def fade_out(&when_done)
    @mode = :fading_out
    @fade_out = {
      :start_time => Time.now,
      :duration   => 1,
      :progress   => 0,
      :alpha      => 0,
      :when_done  => when_done
    }
  end
  
  def present_boss(name, avatar_filename, title = "Boss", duration = 1, &block)
    begin
      title_image = Gosu::Image.from_text(self, title,
                                            BOSS_PRESENTATION_TITLE_FONT,
                                            BOSS_PRESENTATION_TITLE_FONT_SIZE)
    rescue
      title_image = Gosu::Image.from_text(self, title,
                                            Gosu::default_font_name,
                                            BOSS_PRESENTATION_TITLE_FONT_SIZE)
    end
    
    begin
      name_width = Gosu::Image.from_text(self, name, BOSS_PRESENTATION_FONT,
                                           BOSS_PRESENTATION_FONT_SIZE).width
    rescue
      name_width = Gosu::Image.from_text(self, name, Gosu::default_font_name,
                                           BOSS_PRESENTATION_FONT_SIZE).width
    end
    
    fade_out do
      @bg_song.stop
      @mode = :presenting_boss
      @presenting_boss = {
        :name => name,
        :duration => duration,
        :sound => SoundPool.get(self, 'megaman_game_start.ogg').play,
        :avatar => Gosu::Image.new(self, avatar_filename, false),
        :title_image => title_image,
        :name_width => name_width,
        :stars => Gosu::Image.new(self, 'stars.png', false),
        :state => :opening,
        :start_time => Time.now,
        :when_done => block
      }
      update_boss_presentation_progress
    end
  end

  private
  
  def determine_screen_flash(old_health)
    if old_health < @player.health
      # Power-up
      @powerup_screen_flash   = 100
      @powerdown_screen_flash = 0
    elsif old_health > @player.health
      # Power-down
      @powerdown_screen_flash = 100
      @powerup_screen_flash   = 0
    end
  end
  
  def update_fade_out_progress
    if @fade_out
      @fade_out[:progress] = (Time.now - @fade_out[:start_time]) / @fade_out[:duration]
      @fade_out[:alpha] = (255.0 * @fade_out[:progress]).to_i
      if @fade_out[:progress] > 1
        @mode == :normal
        when_done = @fade_out[:when_done]
        @fade_out = nil
        when_done.call
      end
    end
  end
  
  def update_boss_presentation_progress
    args = @presenting_boss
    
    if button_down?(Gosu::Button::KbSpace)
      args[:state] = :done
      args[:start_time] = Time.now
      args[:duration] = 0
      args[:stars_start_time] = Time.now
    end
    
    case args[:state]
    when :opening
      animation_duration = 0.7
      max_background_size = 100
      args[:background_size] = ((Time.now - args[:start_time]) / animation_duration) * max_background_size
      if args[:background_size] >= max_background_size
        args[:state] = :presenting
        args[:start_time] = Time.now
        args[:chars] = 0
        args[:stars_start_time] = Time.now
      end
      
    when :presenting
      chars_per_second = 6
      args[:chars] = ((Time.now - args[:start_time]) * chars_per_second).to_i
      if args[:chars] > args[:name].size
        args[:chars] = args[:name].size 
        args[:state] = :waiting_until_done
      end
    
    when :waiting_until_done
      if !args[:sound].playing?
        args[:state] = :done
        args[:start_time] = Time.now
      end
      
    when :done
      if Time.now - args[:start_time] > args[:duration]
        args[:sound].stop if args[:sound]
        @presenting_boss = nil
        @bg_song.play(true)
        @mode = :normal
        args[:when_done].call if args[:when_done]
      end
    end
    
    if args[:state] != :opening
      args[:stars_pos] = ((Time.now - args[:stars_start_time]) * -200) % args[:stars].width
    end
  end

  # Invoke AI players' AI. Maximum of AI_INVOCATIONS_PER_LOOP AI invocations per call.
  def invoke_players
    if @ai_schedule_index > @map.players.size - 1
      @ai_schedule_index = 0
    end
    
    if !@map.players.empty?
      if @map.players.size > Config::AI_INVOCATIONS_PER_LOOP
        max_num_invoked = Config::AI_INVOCATIONS_PER_LOOP
      else
        max_num_invoked = @map.players.size
      end
      num_invoked = 0
      i = 0
      real_index_of_last_invoked_ai_player = 0
      
      while i < @map.players.size && num_invoked < max_num_invoked
        real_index = (@ai_schedule_index + i) % @map.players.size
        ai_player = @map.players[real_index]
        
        dx = @player.x - ai_player.x
        dy = @player.y - ai_player.y
        
        # Only invoke the AI if the player is sufficiently close to the
        # main character.
        square_distance_to_main_character = dx * dx + dy * dy
        
        if square_distance_to_main_character < (ai_player.sight * Map::GRID_WIDTH_HEIGHT) ** 2
          ai_player.interact(@player, @drawn_sprite_x)
          real_index_of_last_invoked_ai_player = real_index
          num_invoked += 1
        end
        
        i += 1
      end
      
      @ai_schedule_index = (real_index_of_last_invoked_ai_player + 1) % @map.players.size
    end
  end

  def invoke_items
    @map.items.each { |item|
      item.interact(@player)
    }
  end

  def invoke_doors
    current_time = Time.now.to_i

    @map.doors.each_with_index { |doors_row, doors_row_index|
      doors_row.each_with_index { |door, doors_column_index|
        if not door.nil?
          door.interact
          
          row, column = Map.matrixify(@player.y, @player.x)

          d_row    = row - doors_row_index
          d_column = column - doors_column_index
          r_2 = (d_row * d_row) + (d_column * d_column)
          r_2 = (Door::FULL_VOLUME_WITHIN_GRID_BLOCKS * Door::FULL_VOLUME_WITHIN_GRID_BLOCKS) if r_2 == 0
          
          door_close_sound_volume = (Door::FULL_VOLUME_WITHIN_GRID_BLOCKS * Door::FULL_VOLUME_WITHIN_GRID_BLOCKS) / r_2
          door_close_sound_volume = 1.0 if door_close_sound_volume > 1.0

          if door.open? && @map.doors[row][column] != door && (current_time - door.opened_at) >= Door::STAYS_SECONDS_OPEN
            @door_close_sound.play(door_close_sound_volume) if door_close_sound_volume > 0
            door.close!
          end
        end
      }
    }
  end

  def process_movement_input
    @player.turn_left  if button_down? Gosu::Button::KbLeft
    @player.turn_right if button_down? Gosu::Button::KbRight
    @player.move_forward  if button_down? Gosu::Button::KbUp and @player.can_move_forward?(@map)
    @player.move_backward if button_down? Gosu::Button::KbDown and @player.can_move_backward?(@map)
    
    if button_down? Gosu::Button::KbSpace
      column, row = Map.matrixify(@player.x, @player.y)
      door = @map.get_door(row, column, @player.angle)
      
      if !door.nil?
        if door.open?
          @door_close_sound.play
          door.close!
        elsif door.closed?
          @door_open_sound.play
          door.open!
        end
        return
      end
      
      sprite_in_crosshair = @drawn_sprite_x[Config::WINDOW_WIDTH/2]
      
      if sprite_in_crosshair && sprite_in_crosshair.respond_to?(:take_damage_from) && sprite_in_crosshair.respond_to?(:dead?) && !sprite_in_crosshair.dead?
        sprite_in_crosshair.take_damage_from(@player)
      end
      
      @fired_weapon = true
    else
      @fired_weapon = false
    end
  end
  
  def button_down(id)
    if id == Gosu::Button::KbEscape
      close
    end
  end

  def draw_sprites
    @drawn_sprite_x.clear
    #@sprite_in_crosshair = nil
    
    @map.sprites.each { |sprite|
      dx = (sprite.x - @player.x)
      # Correct the angle by mirroring it in x. This is necessary seeing as our grid system increases in y when we "go down"
      dy = (sprite.y - @player.y) * -1
      
      distance = Math.sqrt( dx ** 2 + dy ** 2 )
      
      sprite_angle = (Math::atan2(dy, dx) * 180 / Math::PI) - @player.angle
      # Correct the angle by mirroring it in x. This is necessary seeing as our grid system increases in y when we "go down"
      sprite_angle *= -1
      
      perp_distance = ( distance * Math.cos( sprite_angle * Math::PI / 180 ))#.abs
      next if perp_distance <= 0 # Behind us... no point in drawing this.

      sprite.z_order = ZOrder::SPRITES + ( 1 / (perp_distance / Map::GRID_WIDTH_HEIGHT))
      sprite_pixel_factor = ( Player::DISTANCE_TO_PROJECTION / perp_distance )
      sprite_size = sprite_pixel_factor * Sprite::TEX_WIDTH
      
      x = ( Math.tan(sprite_angle * Math::PI / 180) * Player::DISTANCE_TO_PROJECTION + (Config::WINDOW_WIDTH - sprite_size) / 2).to_i
      next if x + sprite_size.to_i < 0 or x >= Config::WINDOW_WIDTH # Out of our screen resolution

      y = (Config::WINDOW_HEIGHT - sprite_size) / 2
      
      i = 0
      slices = sprite.slices
      
      while(i < Sprite::TEX_WIDTH && (i * sprite_pixel_factor) < sprite_size)
        slice = x + i * sprite_pixel_factor
        slice_idx = slice.to_i
        
        if slice >= 0 && slice < Config::WINDOW_WIDTH && perp_distance < @wall_perp_distances[slice_idx]
          slices[i].draw(slice, y, sprite.z_order, sprite_pixel_factor, sprite_pixel_factor, 0xffffffff)
          drawn_slice_idx = slice_idx
          
          if sprite.respond_to?(:dead?) && !sprite.dead?
            old_sprite = @drawn_sprite_x[drawn_slice_idx]
            old_sprite_is_alive_and_in_front_of_sprite = old_sprite && old_sprite.z_order > sprite.z_order && old_sprite.respond_to?(:dead?) && !old_sprite.dead?
            
            if not old_sprite_is_alive_and_in_front_of_sprite
              while(drawn_slice_idx < (slice + sprite_pixel_factor))
                # Fill up all the @drawn_sprite_x buffer with current sprite till the next sprite_pixel_factor
                @drawn_sprite_x[drawn_slice_idx] = sprite
                drawn_slice_idx += 1
              end
            end
          end
        end
        
        i += 1
      end
    }
    
  end

  def draw_scene
    @floor_ceil.draw(0, 0, ZOrder::BACKGROUND)
    
    # Raytracing logics
    ray_angle         = (360 + @player.angle + (Player::FOV / 2)) % 360
    ray_angle_delta   = Player::RAY_ANGLE_DELTA
    
    slice = 0
    while slice < Config::WINDOW_WIDTH
    
      type, distance, map_x, map_y = @map.find_nearest_intersection(@player.x, @player.y, ray_angle)
      
      # Correct spherical distortion
      # corrected_distance here is the perpendicular distance between the player and wall.
      corrected_angle = ray_angle - @player.angle
      corrected_distance = distance * Math::cos(corrected_angle * Math::PI / 180)
      
      slice_height = ((Map::TEX_HEIGHT / corrected_distance) * Player::DISTANCE_TO_PROJECTION)
      slice_y = (Config::WINDOW_HEIGHT - slice_height) * (1 - @player.height)
            
      n = 0
      while n < Config::SUB_DIVISION && (slice + n) < Config::WINDOW_WIDTH
        @wall_perp_distances[slice + n] = corrected_distance
        texture = @map.texture_for(type, map_x, map_y, ray_angle)
        texture.draw(slice + n, slice_y, ZOrder::LEVEL, 1, slice_height / Map::TEX_HEIGHT) if texture
        
        ray_angle = (360 + ray_angle - ray_angle_delta) % 360
        n += 1
      end
      
      slice += (n == 0) ? 1 : n
    end
  end

  def draw_hud
    @hud.draw(0, 405, ZOrder::HUD)
    if @player.health_percent <= 85 && @player.health_percent > 70
      portret_id = 1
    elsif @player.health_percent <= 70 && @player.health_percent > 55
      portret_id = 2
    elsif @player.health_percent <= 55 && @player.health_percent > 40
      portret_id = 3
    elsif @player.health_percent <= 40 && @player.health_percent > 25
      portret_id = 4
    elsif @player.health_percent <= 25 && @player.health_percent > 10
      portret_id = 5
    elsif @player.health_percent <= 10
      portret_id = 6
    else
      portret_id = 0
    end
    
    @hud_portret[portret_id].draw(268, 414, ZOrder::HUD)
    # Health
    draw_number(@player.health, 375)
    # Score
    draw_number(@player.score, 178)
  end

  def draw_number(number, x, y = 435)
    n = 1
    while (number == 0 && n == 1) || n <= number
      digit = (number / n).to_i
      digit %= 10
      
      @hud_numbers[digit].draw(x, y, ZOrder::HUD + 1)
      
      x -= 16
      
      n *= 10
    end
  end

  def draw_weapon
    if button_down? Gosu::Button::KbUp
      dy = Math.cos(Time.now.to_f * -10) * 7
    elsif button_down? Gosu::Button::KbDown
      dy = Math.cos(Time.now.to_f * 10) * 7
    else
      dy = Math.cos(Time.now.to_f * 5) * 3
    end
    
    if @fired_weapon
      @weapon_fire.draw(200, 240 + dy, ZOrder::WEAPON)
      @fire_sound.play(0.2)
    else
      @weapon_idle.draw(200, 276 + dy, ZOrder::WEAPON)
    end
  end

  def draw_screen_flash
    if @powerdown_screen_flash > 0 || @powerup_screen_flash > 0
      if @powerdown_screen_flash > 0
        screen_flash_color = POWERDOWN_SCREEN_FLASH_COLOR
        screen_flash_color.alpha = @powerdown_screen_flash
        @powerdown_screen_flash -= SCREEN_FLASH_STEP
      elsif @powerup_screen_flash > 0
        screen_flash_color = POWERUP_SCREEN_FLASH_COLOR
        screen_flash_color.alpha = @powerup_screen_flash
        @powerup_screen_flash -= SCREEN_FLASH_STEP
      end
      
      draw_quad(
        TOP, LEFT, screen_flash_color, RIGHT, TOP,
        screen_flash_color, RIGHT, BOTTOM, screen_flash_color,
        LEFT, BOTTOM, screen_flash_color, ZOrder::SCREEN_FLASH
      )
    end
  end
  
  def draw_text
    if @active_text
      if Time.now > @active_text_timeout
        @active_text = nil
        @active_text_timeout = nil
      else
        images  = ImagePool.get_text(self, @active_text)
        y       = 12
        bg_top  = y
        bg_left = bg_right = bg_bottom = nil
        
        images.each do |image|
          x = (RIGHT - LEFT) / 2 - image.width / 2
          image.draw(x, y, ZOrder::TEXT)
          y += image.height + TEXT_VERTICAL_SPACING
          
          bg_left = x if bg_left.nil? || x < bg_left
          bg_right = x + image.width if bg_right.nil? || x + image.width > bg_right
        end
        bg_bottom = y - TEXT_VERTICAL_SPACING
        
        bg_left   -= TEXT_BACKGROUND_PADDING
        bg_right  += TEXT_BACKGROUND_PADDING
        bg_top    -= TEXT_BACKGROUND_PADDING
        bg_bottom += TEXT_BACKGROUND_PADDING
        
        draw_quad(bg_left, bg_top, TEXT_BACKGROUND_COLOR,
                  bg_right, bg_top, TEXT_BACKGROUND_COLOR,
                  bg_right, bg_bottom, TEXT_BACKGROUND_COLOR,
                  bg_left, bg_bottom, TEXT_BACKGROUND_COLOR,
                  ZOrder::TEXT_BACKGROUND)
      end
    end
  end
  
  def draw_fade_out_overlay
    if @fade_out
      FADE_OUT_OVERLAY_COLOR.alpha = @fade_out[:alpha]
      draw_quad(LEFT,  TOP, FADE_OUT_OVERLAY_COLOR,
                RIGHT, TOP, FADE_OUT_OVERLAY_COLOR,
                RIGHT, BOTTOM, FADE_OUT_OVERLAY_COLOR,
                LEFT,  BOTTOM, FADE_OUT_OVERLAY_COLOR,
                ZOrder::FADE_OUT_OVERLAY)
    end
  end
  
  def draw_boss_presentation
    if @presenting_boss
      args = @presenting_boss
      
      top = (BOTTOM - TOP) / 2 - args[:background_size] / 2
      bottom = top + args[:background_size]
      
      draw_quad(LEFT, top - 50, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, top - 50, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, top - 48, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                LEFT, top - 48, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                1)
      draw_quad(LEFT, top - 25, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, top - 25, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, top - 23, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                LEFT, top - 23, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                1)
      draw_quad(LEFT, top - 15, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                RIGHT, top - 15, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                RIGHT, top - 10, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                LEFT, top - 10, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                1)
      draw_quad(LEFT, top - 7, BOSS_PRESENTATION_BACKGROUND_COLOR,
                RIGHT, top - 7, BOSS_PRESENTATION_BACKGROUND_COLOR,
                RIGHT, top - 4, BOSS_PRESENTATION_BACKGROUND_COLOR,
                LEFT, top - 4, BOSS_PRESENTATION_BACKGROUND_COLOR,
                1)
      draw_quad(LEFT, top, BOSS_PRESENTATION_BACKGROUND_COLOR,
                RIGHT, top, BOSS_PRESENTATION_BACKGROUND_COLOR,
                RIGHT, bottom, BOSS_PRESENTATION_BACKGROUND_COLOR,
                LEFT, bottom, BOSS_PRESENTATION_BACKGROUND_COLOR,
                1)
      draw_quad(LEFT, bottom + 50, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, bottom + 50, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, bottom + 48, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                LEFT, bottom + 48, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                1)
      draw_quad(LEFT, bottom + 25, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, bottom + 25, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                RIGHT, bottom + 23, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                LEFT, bottom + 23, BOSS_PRESENTATION_WHITE_LINE_COLOR,
                1)
      draw_quad(LEFT, bottom + 15, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                RIGHT, bottom + 15, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                RIGHT, bottom + 10, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                LEFT, bottom + 10, BOSS_PRESENTATION_CYAN_LINE_COLOR,
                1)
      draw_quad(LEFT, bottom + 7, BOSS_PRESENTATION_BACKGROUND_COLOR,
                RIGHT, bottom + 7, BOSS_PRESENTATION_BACKGROUND_COLOR,
                RIGHT, bottom + 4, BOSS_PRESENTATION_BACKGROUND_COLOR,
                LEFT, bottom + 4, BOSS_PRESENTATION_BACKGROUND_COLOR,
                1)
      
      if args[:state] == :presenting || args[:state] == :waiting_until_done || args[:state] == :done
        args[:stars].draw(args[:stars_pos], 0, 2)
        args[:stars].draw(args[:stars_pos] - args[:stars].width, 0, 2)
        args[:stars].draw(args[:stars_pos] + args[:stars].width, 0, 2)
        args[:stars].draw(args[:stars_pos], BOTTOM - args[:stars].height, 2)
        args[:stars].draw(args[:stars_pos] - args[:stars].width, BOTTOM - args[:stars].height, 2)
        args[:stars].draw(args[:stars_pos] + args[:stars].width, BOTTOM - args[:stars].height, 2)
        
        args[:title_image].draw((RIGHT - LEFT) / 2 - args[:title_image].width / 2,
                                top - args[:title_image].height - 80, 3)
        
        begin
          image = Gosu::Image.from_text(self, args[:name][0 .. args[:chars]],
                      BOSS_PRESENTATION_FONT, BOSS_PRESENTATION_FONT_SIZE)
        rescue
          image = Gosu::Image.from_text(self, args[:name][0 .. args[:chars]],
                      Gosu::default_font_name, BOSS_PRESENTATION_FONT_SIZE)
        end
        
        image.draw((RIGHT - LEFT) / 2 - args[:name_width] / 2, bottom + 80, 3)
      end
      if args[:state] == :waiting_until_done || args[:state] == :done
        args[:avatar].draw((RIGHT - LEFT) / 2 - args[:avatar].width / 2,
                           (BOTTOM - TOP) / 2 - args[:avatar].height / 2,
                           2)
      end
    end
  end
  
end

game_window = GameWindow.new
if ARGV[0] == '--profile'
  require 'ruby-prof'
  result = RubyProf.profile do
    game_window.show
  end
  File.open('profile.html', 'w') do |f|
    RubyProf::GraphHtmlPrinter.new(result).print(f, :min_percent => 5)
  end
else
  game_window.show
end

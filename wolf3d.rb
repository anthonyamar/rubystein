#!/usr/bin/env ruby
require 'rubygems'
require 'gosu'

require 'map'
require 'player'

module ZOrder
  BACKGROUND = 0
  LEVEL   = 1
  OBJECTS = 2
  ENEMIES = 3
  HUD     = 10
end

class GameWindow < Gosu::Window
  # TODO abstract functionality of controller in a module and mixin
  WINDOW_WIDTH  = 640
  WINDOW_HEIGHT = 480
  FULLSCREEN    = true
  
  def initialize
    super(WINDOW_WIDTH, WINDOW_HEIGHT, FULLSCREEN)
    self.caption = 'Rubenstein 3d by Phusion CS Company'
    @font = Gosu::Font.new(self, Gosu::default_font_name, 20)
    
    @map = Map.new([
        # Top left element represents (x=0,y=0)
        [1, 1, 1, 1, 1, 1, 1, 1],
        [1, 0, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 2, 1, 1, 1],
        [1, 0, 0, 0, 0, 1, 0, 1],
        [1, 0, 5, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 6, 4, 3, 1],
        [1, 0, 0, 0, 0, 0, 0, 1],
        [1, 1, 1, 1, 1, 1, 1, 1]],
        [
          { :north => 'blue1_1.png', :east => 'blue1_2.png', :south => 'blue1_1.png', :west => 'blue1_2.png' },
          { :north => 'grey1_1.png', :east => 'grey1_2.png', :south => 'grey1_1.png', :west => 'grey1_2.png' },
          { :north => 'wood1_1.png', :east => 'wood1_2.png', :south => 'wood1_1.png', :west => 'wood1_2.png' },
          { :north => 'wood_php_1.png', :east => 'wood_php_1.png', :south => 'wood_php_1.png', :west => 'wood_php_1.png' },
          { :north => 'blue2_1.png', :east => 'blue1_2.png', :south => 'blue1_1.png', :west => 'blue1_2.png' },
          { :north => 'blue3_1.png', :east => 'blue3_2.png', :south => 'blue3_1.png', :west => 'blue3_2.png' }
        ],
        self
    )
    
    @player = Player.new
    @player.height = 0.5
    @player.x = 96
    @player.y = 96
    @player.angle = 0
    
    @floor_ceil = Gosu::Image::new(self, 'floor_ceil.png', true)
  end

  def update
    process_movement_input
  end

  def process_movement_input
    @player.turn_left  if button_down? Gosu::Button::KbLeft
    @player.turn_right if button_down? Gosu::Button::KbRight
    @player.move_forward  if button_down? Gosu::Button::KbUp and @player.can_move_forward?(@map)
    @player.move_backward if button_down? Gosu::Button::KbDown and @player.can_move_backward?(@map)
  end
  
  def button_down(id)
    if id == Gosu::Button::KbEscape
      close
    end
  end

  def draw
    @floor_ceil.draw(0, 0, ZOrder::BACKGROUND)
    
    # Raytracing logics
    ray_angle         = (360 + @player.angle + (Player::FOV / 2)) % 360
    ray_angle_delta   = Player::RAY_ANGLE_DELTA
    
    for slice in 0...WINDOW_WIDTH
      type, distance, map_x, map_y = @map.find_nearest_intersection(@player.x, @player.y, ray_angle)
      
      # Correct spherical distortion
      corrected_angle = ray_angle - @player.angle
      corrected_distance = distance * Math::cos(corrected_angle * Math::PI / 180)
      
      slice_height = ((Map::TEX_HEIGHT / corrected_distance) * Player::DISTANCE_TO_PROJECTION)
      slice_y = (WINDOW_HEIGHT - slice_height) * (1 - @player.height)
      
      texture = @map.texture_for(type, map_x, map_y, ray_angle)
      texture.draw(slice, slice_y, ZOrder::LEVEL, 1, slice_height / Map::TEX_HEIGHT)
      
      ray_angle = (360 + ray_angle - ray_angle_delta) % 360
    end
  end
  
end

game_window = GameWindow.new
game_window.show
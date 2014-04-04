##########################################################################
#File name: mapObjects.py
#Author: Klemente Gilbert-Espada										
#Description: This file contains the definitions of the different classes
#that comprise the dungeon itself (individual squares, terrain types, etc)
##########################################################################

import curses, characters, items
#The terrainType class is going to be the parent class
#for the different types of terrain that tiles can have
class TerrainType:
	#terrain types have a symbol that identifies them on the map
	symbol = ''
	#terrain types are either passable or not passable by the character
	passable = False
	#terrain types are displayed with a corresponding color
	color = curses.COLOR_WHITE

#Our floor class is going to be the primary type of terrain the player will travel on
class Floor (TerrainType):
	symbol = '.'
	#the player can walk on floors
	passable = True

#Our wall class is a type of terrain that will be placed next to floor tiles
class Wall (TerrainType):
	symbol = '#'
	#the player can't walk through walls
	passable = False

class UpStairs (TerrainType):
	symbol = '<'
	passable = True

class DownStairs (TerrainType):
	symbol = '>'
	passable = True

#The tile class represents a particular square on the map,
#it can be occupied by characters and contain items,
#it has a terrain type
class Tile:
	visible = False
	explored = False
	#tiles start empty of characters
	character = characters.NullCharacter()
	#tiles start empty of items
	terrain = TerrainType()

	#This value will be manipulated by our pathfinding algorithm
	pathValue = 0

	#our initialization function takes a terrain type,
	#when we initialize the tile we give it that type
	def __init__(self, terrain, y, x):
		self.terrain = terrain
		self.yPos = y
		self.xPos = x
		self.contents = []

	#when we want to print our tile, generally we want to know what 
	#its symbol is and what its color is.
	#our printTile function returns those values
	#paramaters: None
	#returns: a tuple with the symbol the tile will display and the color
	#of that symbol
	def printTile(self):
		#we check to see if there's a visible character in the tile
		if self.character.symbol != '':
			#If there is a character in the tile, we return the 
			#symbol and color provided by that character
			symbol = self.character.symbol
			color = self.character.color
		
		#If there's no character, but there are items, return the first
		#item's symbol and color
		elif self.contents:
			symbol = self.contents[0].symbol
			color = self.contents[0].color
			
		else: 
			#If the tile is empty, we'll return the
			#symbol provided by the terrain
			symbol = self.terrain.symbol
			color = self.terrain.color

		if not self.visible:
			#if the square is not visible, 
			#magenta is the color we're using to
			#denote not seeing it currently
			color = curses.COLOR_MAGENTA
			
		#If the player hasn't seen it yet, we won't print it.
		if not self.explored:
			symbol = ''

		return [symbol, color]

	#listAdjacentTiles returns a list of tiles that are adjacent to the tile
	#calling the function
	#Parameters: 	A map of the level.
	#Returns: 		A list of adjacent tiles
	def listAdjacentTiles(self, levelMap):
		#For brevity
		y = self.yPos
		x = self.xPos
		#Initialize our new list
		adjacentTiles = []

		#If there is a tile above us, add it to the list.
		if y > 0:
			adjacentTiles.append(levelMap[y - 1][x])		#Up

		#If there is a tile below us, add it to the list.
		if y < len(levelMap) - 1:
			adjacentTiles.append(levelMap[y + 1][x])		#Down

		#If there is a tile to our right, add it to the list.
		if x < len(levelMap[y]) - 1:
			adjacentTiles.append(levelMap[y][x + 1])		#Right

		#If there is a tile to our left, add it to the list.
		if x > 0:
			adjacentTiles.append(levelMap[y][x - 1])		#Left

		#You get the idea
		if y < len(levelMap) - 1 and x < len(levelMap[y]) - 1:
			adjacentTiles.append(levelMap[y + 1][x + 1])	#DownRight
		
		if y > 0 and x < len(levelMap[y]) - 1:
			adjacentTiles.append(levelMap[y -1][x + 1])		#UpRight
		
		if y > 0 and x > 0:
			adjacentTiles.append(levelMap[y - 1][x - 1])	#UpLeft

		if y < len(levelMap) - 1 and x > 0:
			adjacentTiles.append(levelMap[y + 1][x - 1])	#DownLeft

		#Return our list
		return adjacentTiles


#our level class represents a specific level of the dungeon.
#it has a level number, and a map of the tiles on the level.
class Level:
	
	#Our intialization function takes a string that represents
	#a map of a level. Rows of the map are seperated by the /
	#character, and individual spaces are seperated by whitespace
	def __init__(self, mapString):
		#Our level map starts as an empty list
		self.levelMap = []
		#Our characters array will keep track of the characters
		#currently running around on the level.
		self.characters = []
		#On the other hand, our newCharacters list keeps track
		#of what characters we'll be initializing and where once the
		#map is finished building.
		newCharacters = []

		#We're starting at the top, and making rows
		#as we go down.
		y = 0
		#split our input string into rows,
		#and then iterate across them
		for row in mapString.split('/'):
			#create an empty list to append spaces to
			tileRow = []
			#iterate across each row
			x = 0
			for space in row.split():
				#identify what type each character
				#signifies, and add whatever that character 
				#represents to our row.
				
				#A wall.
				if space == "#":
					tileRow.append(Tile(Wall, y, x))
				
				#An empty tile.
				elif space == ".":
					tileRow.append(Tile(Floor, y, x))

				#A staircase going up.
				elif space == "<":
					tileRow.append(Tile(UpStairs, y, x))
					#Mark out the coordinates of our staircase,
					#so that when the player comes up the stairs above us,
					#we will know where to put them.
					self.UpCoordinates = [y, x]
				
				#A staircase going down.
				elif space == ">":
					tileRow.append(Tile(DownStairs, y, x))
					#Mark out the coordinates of our staircase,
					#so that when the player comes up the stairs beneath us,
					#we will know where to put them.
					self.DownCoordinates = [y, x]

				#A space goblin.
				elif space == "g":
					#Put a space goblin onto the list of characters to initialize
					newCharacters.append(["SpaceGoblin", y, x])
					#append a tile with an empty floor for now
					tileRow.append(Tile(Floor, y, x))
				
				#Our player character
				elif space == "@":
					#Put our player onto the list of characters to initialize.
					newCharacters.append(["Player", y, x])
					#append an empty floor tile
					tileRow.append(Tile(Floor, y, x))

				#A potion
				elif space == "%":
					#Create our new tile.
					newTile = Tile(Floor, y, x)
					#Put a potion on it.
					newTile.contents.append(items.Potion())
					#Append this tile to the row.
					tileRow.append(newTile)

				else:
					#if the character is unrecognized,
					#initialize an empty tile 
					#(nothing in it, basic terrain type, which is impassable)
					tileRow.append(Tile(TerrainType, y, x))
				#Increment our x counter (we're moving to the next tile in the row)
				x += 1
			#Once we're done populating the row,
			#we append it to our levelMap
			self.levelMap.append(tileRow)
			#Then we move down and start populating our next row
			y += 1

		#After we've built the dungeon, we initialize the characters where
		#they're supposed to go.
		while newCharacters:
			#If there are still characters to initialize, grab one
			newChar = newCharacters.pop()
			#If it's name is "SpaceGoblin", initialize a space goblin
			#at the y and x coordinates we have. (NewChar[2] and [3] respectively)
			if newChar[0] == "SpaceGoblin":
				characters.SpaceGoblin(self, newChar[1], newChar[2])
			#Otherwise if we're initializing a player, we do that in the same way.
			elif newChar[0] == "Player":
				characters.Player(self, newChar[1], newChar[2])

	#Our clearTileValues function goes through the level and resets all the little
	#properties on every tile.
	def clearTileValues(self):
		#Go through the map
		for row in self.levelMap:
			#For each tile
			for tile in row:
				#Reset everything
				tile.pathValue = 0
				tile.visible = False
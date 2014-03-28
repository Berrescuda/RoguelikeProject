from random import randint
import traceback

class Room:
	width = 0
	height = 0
	xPos = 0
	yPos = 0
	connected = False

	def randomize (self, mapWidth, mapHeight):
		self.width = randint(3, 6)
		self.height = randint(3, 6)
		self.xPos = randint(0, mapWidth -self.width)
		self.yPos = randint(0, mapHeight - self.height)

	def placeInRoom(self, symbol, levelMap):
		y = self.yPos + randint(1, self.height -1)
		x = self.xPos + randint(1, self.width -1)
		if levelMap[y][x] == '.':
			levelMap[y][x] = symbol


	def drawRoom(self, levelMap):
		for y in range(self.height):
			for x in range(self.width):
				levelMap[self.yPos + y][self.xPos + x] = '.'

	def connect(self, room, levelMap):
#		entrance = ''
#		xDirection = ''
#		yDirection = ''

		if room.xPos > self.xPos:
			xDirection = 'east'
		else:
			xDirection = 'west'
		if room.yPos < self.yPos:
			yDirection = 'north'
		else: 
			yDirection = 'south'

		if coinFlip():
			entrance = (yDirection)
			if entrance == 'north':
				corridor = [self.yPos, randint(self.xPos, self.xPos + self.width)]
				goNorth(corridor, room, levelMap)
			else:
				corridor = [self.yPos + self.height, randint(self.xPos, self.xPos + self.width)]
				goSouth(corridor, room, levelMap)	

			if xDirection == 'west':
					goWest(corridor, room, levelMap)
			else: 
				goEast(corridor, room, levelMap)

		else:
			entrance = (xDirection)
			if entrance == 'west':
				corridor = [randint(self.yPos, self.yPos + self.height), self.xPos]
				goWest(corridor, room, levelMap)
			else:
				corridor = [randint(self.yPos, self.yPos + self.height), self.xPos + self.width]
				goEast(corridor, room, levelMap)	

			if yDirection == 'north':
					goNorth(corridor, room, levelMap)
			else: 
				goSouth(corridor, room, levelMap)
		self.connected = True


def wallAdjacentSpaces(x, y, levelMap):
		adjacentSpaces = []
		shouldBeWall = 8
		if y > 0:
			adjacentSpaces.append([y - 1, x])		#Up
			shouldBeWall -= 1
			
		if y < len(levelMap) - 1:				#Down
			adjacentSpaces.append([y + 1, x])
			shouldBeWall -= 1

		if x < len(levelMap[y]) - 1:
			adjacentSpaces.append([y, x + 1])		#Right
			shouldBeWall -= 1

		if x > 0:
			adjacentSpaces.append([y, x - 1])		#Left
			shouldBeWall -= 1

		if y < len(levelMap) - 1 and x < len(levelMap[y]) - 1:
			adjacentSpaces.append([y + 1, x + 1])	#DownRight
			shouldBeWall -= 1
		
		if y > 0 and x < len(levelMap[y]) - 1:
			adjacentSpaces.append([y -1, x + 1])		#UpRight
			shouldBeWall -= 1
		
		if y > 0 and x > 0:
			adjacentSpaces.append([y - 1, x - 1])	#UpLeft
			shouldBeWall -= 1

		if y < len(levelMap) - 1 and x > 0:
			adjacentSpaces.append([y + 1, x - 1])	#DownLeft
			shouldBeWall -= 1

		if shouldBeWall > 0:
			levelMap[y][x] = '#'
			return 1
		for adjacentSpace in adjacentSpaces:
			if levelMap[adjacentSpace[0]][adjacentSpace[1]] == '':
				levelMap[adjacentSpace[0]][adjacentSpace[1]] = '#'



def coinFlip():
	if randint(0,1) == 1:
		return True
	else: 
		return False

def goNorth(corridor, target, levelMap):
	while corridor[0] > (target.yPos + target.height - 1):
		levelMap[corridor[0]][corridor[1]] = '.'
		corridor[0] -= 1

def goSouth(corridor, target, levelMap):
	while corridor[0] < (target.yPos + 1):
		debug = corridor
		levelMap[corridor[0]][corridor[1]] = '.'
		corridor[0] += 1

def goEast(corridor, target, levelMap):
	while corridor[1] < (target.xPos + 1):
			levelMap[corridor[0]][corridor[1]] = '.'
			corridor[1] += 1

def goWest(corridor, target, levelMap):
	while corridor[1] > (target.xPos + target.width - 1):
		levelMap[corridor[0]][corridor[1]] = '.'
		corridor[1] -= 1

def levelMapToString(levelMap):
	mapString = ''
	for y in range(len(levelMap)):
		for x in range(len(levelMap[y])):
			if levelMap[y][x] == '':
				levelMap[y][x] = 'e'
			mapString += levelMap[y][x] + ' '
		mapString += '/'
	return mapString

def addWalls(levelMap):
	y = 0
	for row in levelMap:
		x = 0
		for space in row:
			if space == '.':
				wallAdjacentSpaces(x, y, levelMap)
			x += 1
		y += 1

def generateLevel(mapWidth, mapHeight):
	levelMap = []
	unconnectedRooms = []
	connectedRooms = []
	for y in range(mapHeight):
		row = []
		for x in range(mapWidth):
			row.append('')
		levelMap.append(row)
	roomsOnLevel = randint(6, 12)
	for i in range(roomsOnLevel):
		newRoom = Room()
		newRoom.randomize(mapWidth, mapHeight)
		newRoom.drawRoom(levelMap)
		unconnectedRooms.append(newRoom)

	connectedRooms.append(unconnectedRooms.pop())
	connectedRooms[0].connected = True

	while unconnectedRooms:
		room = unconnectedRooms.pop()
		room.connect(connectedRooms[randint(0, len(connectedRooms)) -1], levelMap)
		connectedRooms.append(room)

	addWalls(levelMap)
	connectedRooms[0].placeInRoom("@", levelMap)
	for room in connectedRooms:
		if coinFlip():
			room.placeInRoom("g", levelMap)

	return levelMapToString(levelMap)

try:
	debug = []
	generateLevel(16, 32)
	print debug
except:
	traceback.print_exc()
	print debug

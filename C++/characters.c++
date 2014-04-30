/////////////////////////////////////////////////////////////////////////////
//File name: characters.cpp
//Author: Klemente Gilbert-Espada										
//Description: This file contains the class definitions for the living
//entities inside the dungeon. The Player class and Monster classes are 
//defined here as subclasses of a parent "Character" class
//When more monsters are added to the game, they will be added here
//as classes that inherit from the "Monster" class
////////////////////////////////////////////////////////////////////////////
using namespace std;

NullCharacter::NullCharacter(void){
	symbol = ' ';
	name = "NULL";
}

Player::Player (int y, int x){
	//Set up our player initializer
	maxHp = 10;
	currentHp = maxHp;
	symbol = '@';
	name = "foobar";
	color = COLOR_RED;
	isPlayer = true;
	power = 1;
	xPos = x;
	yPos = y;
	xp = 0;
	//currentLevel = level;
}

SpaceGoblin::SpaceGoblin(int y, int x, Level* level){
	xPos = x;
	yPos = y;
	maxHp = 5;
	currentHp = maxHp;
	symbol = 'g';
	name = "Space Goblin";
	color = COLOR_RED;
	isPlayer = false;
	power = 1;
	currentLevel = level;
	xp = 0;
}


int Character::move(int y, int x){
	Tile* targetTile = &currentLevel->levelMap[yPos + y][xPos + x];
	if(targetTile->character.symbol != ' '){
		attack(&targetTile->character);
	} else {
		if(targetTile->terrain.passable){
			currentLevel->levelMap[yPos][xPos].character = NullCharacter();
			yPos += y;
			xPos += x;
			currentLevel->levelMap[yPos][xPos].character = *this;
		}
	}
}


void Character::attack(Character* target){
	log("you attack the " + target->name);
	target->currentHp -= power;
	log("hp = " + to_string(target->currentHp));
	if(target->currentHp < 1){
		target->die(this);
	}
}

void Character::die(Character* killer){
	log(name + " dies");
	Tile* currentTile = getTile();
	currentTile->character = NullCharacter();
	xPos = 0;
	yPos = 0;
	killer->xp += 1;
}

Tile* Character::getTile(void){
	return &currentLevel->levelMap[yPos][xPos];
}

void Player::takeTurn(char c){
		bool passTurn = false;

		int direction[2] = {0, 0};

		switch(c){
			case 56:
			case 'w':
				direction[0] = -1;
				break;

			case 50:
			case 's':
				direction[0] = 1;
				break;

			case 52:
			case 'a':
				direction[1] = -1;
				break;

			case 54:
			case 'd':
				direction[1] = 1;
				break;

			case 49:
				direction[0] = 1;
				direction[1] = -1;
				break;
			case 51:
				direction[0] = 1;
				direction[1] = 1;
				break;

			case 55:
				direction[0] = -1;
				direction[1] = -1;
				break;

			case 57:
				direction[0] = -1;
				direction[1] = 1;
				break;

			case 53:
				passTurn = true;
		}
		if(direction[0] != 0 || direction[1] != 0){
			move(direction[0], direction[1]);
			passTurn = true;
		}
	}

/*
int* Monster::findPath(Tile* target){

		//The origin is the tile our character is standing on.
		Tile* origin = getTile();
		//Give the origin a pathvalue that makes it recognizable
		//to our algorithm.
		origin->pathValue = -1;

		//Origin is now the current square.
		Tile currentTile = *origin;
		//We clear off all of the pathValues that may be left over
		//from all tiles on the level.
		currentLevel->clearTileValues();
		//We have a stack of unexplored tiles that will be queued
		//to be explored.
		stack <Tile*> unexploredTiles;
		//we have a stack of directions that we will set our character's
		//path to, so that they can follow them to the target tile.
		stack <int[2]> directions;

		while(currentTile != target){
			//make a list of adjacent tiles, and go through them to see
			//which ones we've explored
			adjTiles = currentTile.listAdjacentTiles(self.level.levelMap)
			for tile in adjTiles:
				//If we're not going backwards specifically and 
				//the tile is something we can walk on, we assign it
				//a value
	}
/*
				if tile.terrain.passable and tile != origin:
						//If the tile's path value is 0, the tile is unexplored.
						if tile.pathValue == 0:
							//If we're just starting, we have to initialize
							//the adjacent tiles at one (if we set the origin tile to 0
								//it causes problems)
							if currentTile == origin:
								tile.pathValue = 1
							else:
								//We set the adjacent 0's to our current tile's path value
								//+1 this way we know how far every square is from the player
								tile.pathValue = currentTile.pathValue + 1
							
							//We put all tiles who we've given a path value to
							//into the unexplored tiles list, if we don't find
							//our target next to us, we'll look through those tiles
							//and continue searching.
							unexploredTiles.append(tile)

							//If the tile we're iterating over is the target, we're done
							//with this part of the algorithm
							if tile == target:
								currentTile = tile
								break
			//While we haven't found the target tile, and there are still unexplored
			//tiles left, iterate over them.
			if unexploredTiles and currentTile != target:
				currentTile = unexploredTiles.pop(0)
		
		//Then we start at the target tile, and work our way backwards
		//to find the origin, pushing the direction our character will
		//have to go onto the directions list
		while(currentTile != origin):
			//Check adjacent tiles
			adjTiles = currentTile.listAdjacentTiles(self.level.levelMap)
			for tile in adjTiles:
				//find a square adjacent to the current square that 
				//is one fewer spaces away from the origin square
				if tile.pathValue == currentTile.pathValue - 1:
					//if we haven't found the origin, push
					//another set of directions onto the list
					if tile == origin or tile.pathValue > 0:
						y = currentTile.yPos - tile.yPos 
						x = currentTile.xPos - tile.xPos
						directions.append((y, x))
						//If the tile is the origin, we update
						//our character's directions, and quit the function
						if tile == origin:
							self.path = directions
							return 1
						//Otherwise we set this tile to the current tile, 
						//and start the process again
						currentTile = tile
						break

}
*/
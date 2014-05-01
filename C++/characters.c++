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
	stack<Tuple> path;
	alive = true;
}

SpaceGoblin::SpaceGoblin(){
}

int Character::move(Tuple direction){

	Tile* targetTile = &currentLevel->levelMap[yPos + direction.y][xPos + direction.x];
	if(targetTile->character != NULL){
		attack(targetTile->character);
	} else { 	
		if(targetTile->terrain.passable){
			currentLevel->levelMap[yPos][xPos].character = NULL;
			yPos += direction.y;
			xPos += direction.x;
			currentLevel->levelMap[yPos][xPos].character = this;
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
	currentTile->character = NULL;

	if (isPlayer == false){
		Monster* monster = this;
		if(monster->getId())
			currentLevel->monsters[monster->getId()] = NULL;
		killer->xp += 1;
	}else{
		cursesCleanup();
		cout << "Sorry, you were killed by a " << killer->name << endl;
		exit(1);
	}

}

Tile* Character::getTile(void){
	cout << "TODEL" << endl;

	return &currentLevel->levelMap[yPos][xPos];
}

int Monster::getId(void){
	
	for(int i = 0; i < currentLevel->monsters.size(); i++)
		if(currentLevel->monsters[i] == this)
			return i;

	return NULL;

}

bool Player::takeTurn(char c){
		bool passTurn = false;

		Tuple direction = {0, 0};

		switch(c){
			case 56:
			case 'w':
				direction.y = -1;
				break;

			case 50:
			case 's':
				direction.y = 1;
				break;

			case 52:
			case 'a':
				direction.x = -1;
				break;

			case 54:
			case 'd':
				direction.x = 1;
				break;

			case 49:
				direction.y = 1;
				direction.x = -1;
				break;
			case 51:
				direction.y = 1;
				direction.x = 1;
				break;

			case 55:
				direction.y = -1;
				direction.x = -1;
				break;

			case 57:
				direction.y = -1;
				direction.x = 1;
				break;

			case 53:
				passTurn = true;
		}
		if(direction.y != 0 || direction.x != 0){
			move(direction);
			passTurn = true;
		}
	return passTurn;
	}


stack<Tuple> Monster::findPath(Tile* target){
		cout << "TODEL" << endl;

	currentLevel->clearTileValues();
	//The origin is the tile our character is standing on.
	Tile* origin = getTile();

	//Give the origin a pathvalue that makes it recognizable
	//to our algorithm.
	origin->pathValue = -1;

	//Origin is now the current square.
	Tile* currentSquare = origin;
	//We clear off all of the pathValues that may be left over
	//from all tiles on the level.
	//We have a stack of unexplored tiles that will be queued
	//to be explored.
	deque <Tile*> unexploredTiles;
	//we have a stack of directions that we will set our character's
	//path to, so that they can follow them to the target tile.
	stack <Tuple> directions;
	vector<Tile*>::iterator tile;
	vector <Tile*> adjTiles;

	while(currentSquare != target){
		//make a list of adjacent tiles, and go through them to see
		//which ones we've explored

		adjTiles = currentSquare->listAdjacentTiles();

			//for tile in adjTiles
			for(int i = 0; i < adjTiles.size(); i++){
			//If we're not going backwards specifically and 
			//the tile is something we can walk on, we assign it
			//a value

				if (adjTiles[i]->terrain.passable && adjTiles[i] != origin){
					//If the tile's path value is 0, the tile is unexplored.

					if (adjTiles[i]->pathValue == 0){
						//If we're just starting, we have to initialize
						//the adjacent tiles at one (if we set the origin tile to 0
							//it causes problems)
						if (currentSquare == origin)
							adjTiles[i]->pathValue = 1;
						else{
							//We set the adjacent 0's to our current tile's path value
							//+1 this way we know how far every square is from the player
							adjTiles[i]->pathValue = currentSquare->pathValue + 1;
						}
						
						//We put all tiles who we've given a path value to
						//into the unexplored tiles list, if we don't find
						//our target next to us, we'll look through those tiles
						//and continue searching.

						unexploredTiles.push_back(adjTiles[i]);
						//If the tile we're iterating over is the target, we're done
						//with this part of the algorithm
						if (adjTiles[i] == target){
							currentSquare = adjTiles[i];
							break;
						}
					}
				}
			}
		//While we haven't found the target tile, and there are still unexplored
		//tiles left, iterate over them.
		if (0 < unexploredTiles.size() && currentSquare != target){
			currentSquare = unexploredTiles.front();
			unexploredTiles.pop_front();
		}
	}

	//Then we start at the target tile, and work our way backwards
	//to find the origin, pushing the direction our character will
	//have to go onto the directions list
	while(currentSquare != origin){
		//Check adjacent tiles
		adjTiles = currentSquare->listAdjacentTiles();
		//for tile in adjTiles
		for(int i = 0; i < adjTiles.size(); i++){

			//find a square adjacent to the current square that 
			//is one fewer spaces away from the origin square
			if (adjTiles[i]->pathValue == currentSquare->pathValue -1 || adjTiles[i]->pathValue == -1){
				//push another set of directions onto the list
				if (adjTiles[i] == origin || adjTiles[i]->pathValue > 0){
					int y = currentSquare->yPos - adjTiles[i]->yPos;
					int x = currentSquare->xPos - adjTiles[i]->xPos;
					Tuple dir = {y, x};
					directions.push(dir);
					//If the tile is the origin, we update
					//our character's directions, and quit the function
					if (adjTiles[i] == origin){
						currentLevel->clearTileValues();

						return directions;
					}

					//Otherwise we set this tile to the current tile, 
					//and start the process again
					currentSquare = adjTiles[i];
					break;		
				}
			}
		}
	}
}

void Monster::takeTurn(Character target){
	stack <Tuple> foundPath = findPath(target.getTile());
	move(foundPath.top());
	foundPath.pop();
}
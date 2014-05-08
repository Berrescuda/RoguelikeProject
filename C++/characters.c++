//File name: characters.c++
//Author: Klemente Gilbert-Espada										
//Description: This file contains the method definitions for the characters
//defined in characters.h

//Our player initializer sets up the player character object
//Initializes base stats, as well as it's position on the level
//Arguments: y position, x position
Player::Player (int y, int x){
	//Initialize max hp at 10
	maxHp = 10;
	//current hp starts at max
	currentHp = maxHp;
	//The player is represented by an '@' symbol
	symbol = '@';
	//The player's name is foobar for now (because I think I'm funny)
	name = "foobar";
	//The player's symbol will be displayed in red
	color = COLOR_RED;
	//This is in fact the player
	isPlayer = true;
	//Everybody's power is one right now
	power = 1;
	//Initialize the player's position on the x axis
	xPos = x;
	//Initialize the player's position on the y axis
	yPos = y;
	//Player starts with 0 xp
	xp = 0;
	//regeneration counter starts at 0
	regenCounter = 0;
}

//Our Space Goblin initializer creates a Space Goblin
SpaceGoblin::SpaceGoblin(int y, int x, Level* level){
	//Initialize the goblin's x coordinate
	xPos = x;
	//Initialize the goblin's y coordinate
	yPos = y;
	//The goblin has 5 hp
	maxHp = 5;
	//Set hp to the max
	currentHp = maxHp;
	//The goblin will be represented by a lower case g
	symbol = 'g';
	//All Space Goblin's are named "Space Goblin" 
	//(They're not a creative bunch)
	name = "Space Goblin";
	//The goblin will also be red
	color = COLOR_RED;
	//The goblin we are creating is not in fact the player
	isPlayer = false;
	//Everybody's power is 1 right now
	power = 1;
	//Let the goblin know what level it's on
	currentLevel = level;
	//The goblin has no xp when we initialize it
	xp = 0;

//	This should be used in a future update
//	stack<Tuple> path;

	//The goblin's regen counter starts at 0
	regenCounter = 0;
	//The goblin can't see the player until they at least check
	//their line of sight
	canSeePlayer = false;
}

//Our Move function moves our character one square on the level map.
//Parameters: The direction to move in - 
//(a tuple indicating movement in the y and x directions)
void Character::move(Tuple direction){
	//Identify the tile we want to move on to
	Tile* targetTile = &currentLevel->levelMap[yPos + direction.y][xPos + direction.x];
	//If there's a character on that tile, attack that character instead of moving
	if(targetTile->character != NULL){
		attack(targetTile->character);
	} else {
		//Otherwise, move to that tile if it's possible to walk on it 	
		if(targetTile->terrain.passable){
			//The tile you're currently standing on now doesn't hold a character
			currentLevel->levelMap[yPos][xPos].character = NULL;
			//adjust x and y positions of our character
			yPos += direction.y;
			xPos += direction.x;
			//The tile we are now standing on holds this character
			currentLevel->levelMap[yPos][xPos].character = this;
		}
	}
}

//Every character should heal naturally over time
//Every ten turns, a character heals 1 hp
//Parameters: 	None
void Character::healNaturally(){
	//Only increment the regen counter
	//if we're below our max hp
	if (currentHp < maxHp){
		
		//If the regencounter is somehow below 0,
		//something is wrong and this should be adjusted
		if(regenCounter < 0)
			regenCounter = 0;

		//Becauser we have less than max hp,
		//Increment the regen counter
		regenCounter++;

		//If weve been healing ten turns, 
		//Heal 1 hp
		if (regenCounter >= 10){
			currentHp++;
			//Start the counter over again
			regenCounter = 0;
		}
	}
}

//The attack function allows one character to damage another
//Parameters: A pointer to the character we want to attack
void Character::attack(Character* target){
	//Print the attack to the log so the player can follow along
	log(name + " attacks " + target->name);
	//Take a chunk of hp out of the target equal to our power
	target->currentHp -= power;
	//If the target has less than 1 hp, they are dead
	if(target->currentHp < 1){
		//pass a pointer to ourselves along so they know we killed them
		target->die(this);
	}
}

//Our die function handles what happens when our character joins the choir invisible
//Parameters: A pointer to the character that killed us
void Character::die(Character* killer){
	//Print who died to the log
	log(name + " dies");
	//Grab the tile we're standing on
	Tile* currentTile = getTile();
	//We are no longer standing on that tile
	currentTile->character = NULL;

	//If we're not the player, that's good
	if (isPlayer == false){
		//We know we're a monster since we're not the player
		Monster* monster = this;
		//if we have an id
		if(monster->getId())
			//set our entry in the level's monster vector to NULL
			//(this is a hack and should be adjusted)
			currentLevel->monsters[monster->getId()] = NULL;
		//Whoever killed us gets an xp bump
		killer->xp += 1;
	}
	//If we are the player, that's bad
	else{
		//Well the game's over, return the terminal to it's normal state
		cursesCleanup();
		//Might as well let the user know who killed them
		//(Pro tip: It was probably a space goblin)
		cout << "Sorry, you were killed by a " << killer->name << endl;
		//And we're done here
		exit(1);
	}

}

//Our get tile function returns a pointer to the tile the character
//Is currently standing on.
//Returns: A tile pointer to our tile
Tile* Character::getTile(void){
	//because it's easier than typing this over and over again
	return &currentLevel->levelMap[yPos][xPos];
}


//Our getId function scans the current level's monster vector
//for our monster, and then returns our position in that vector
//returns: an int designating our position, or NULL if we can't
//find ourselves
int Monster::getId(void){
	
	//For every item in the monsters vector
	//(This should be updated to use an iterator)
	for(int i = 0; i < currentLevel->monsters.size(); i++)
		//If that monster is us, return the number we're on
		if(currentLevel->monsters[i] == this)
			return i;

	return NULL;
}

//Our takeTurn function handles what happens during the player's turn
//Parameters: 	The key the player pressed this turn
//Returns: 	  	A boolean indicating whether or not the player has 
//				actually taken an in game turn
bool Player::takeTurn(char c){
		//We haven't taken any in game actions yet, so passTurn is set to false
		bool passTurn = false;
		//Get the tile we're standing on for future reference
		Tile* currentTile = getTile();

		//This is the direction we'll be moving if a movement key is pressed
		Tuple direction = {0, 0};
		//A switch to handle different keypresses
		switch(c){
			//The number '8' on the numpad
			case 56:
			//The up arrow key
			case 3:
			//The 'w' key
			case 'w':
				//We're moving straight up
				direction.y = -1;
				break;

			//The number '2' on the numpad
			case 50:
			//the down arrow key
			case 2:
			//The 's' key
			case 's':
				//We're moving straight down
				direction.y = 1;
				break;

			//The number '4' on the numpad
			case 52:
			//The left arrow key
			case 4:
			//The 'a' key
			case 'a':
				//We're headed straight to the left
				direction.x = -1;
				break;

			//The number '6' on the numpad
			case 54:
			//The right arrow key
			case 5:
			//The 'd' key
			case 'd':
				//We're headed straight to the right
				direction.x = 1;
				break;

			//The number '1' on the numpad
			case 49:
				//We're headed diagonally 
				//down and to the left
				direction.y = 1;
				direction.x = -1;
				break;
			//The number '3' on the numpad
			case 51:
				//We're headed diagonally 
				//down and to the right
				direction.y = 1;
				direction.x = 1;
				break;
			//The number '7' on the numpad
			case 55:
				//We're headed diagonally 
				//up and to the left
				direction.y = -1;
				direction.x = -1;
				break;

			//The number '9' on the numpad
			case 57:
				//We're headed diagonally 
				//up and to the right
				direction.y = -1;
				direction.x = 1;
				break;

			//The number '5' on the numpad
			case 53:
				//We wait one turn
				passTurn = true;
				break;

			case 'g':
				//If the tile we're standing on has items on it, we grab one
				if (!currentTile->items.empty()){
					//Push the item into our inventory
					inventory.push_back(currentTile->items.front());
					//Remove the item from the tile
					currentTile->items.erase(currentTile->items.begin());
					//This action takes a turn
					passTurn = true;
				}
				break;

			case 'u':
				//Use a potion in our inventory
				//If there is one, of course
				if(!inventory.empty()){
					//Drink the last potion in our inventory
					inventory[inventory.size() - 1]->drink(this);
					//Remove that item
					inventory.pop_back();
					//This action takes a turn
					passTurn = true;
				}
				break;

			case '>':
				//If we're standing on a down staircase, go down
				if(currentTile->terrain.symbol == '>'){
					//Go to the next level of the dungeon
					currentLevel = dungeon->level[currentLevel->levelNumber + 1];
					//Remove ourselves from the tile we're standing on
					currentTile->character = NULL;
					//Update our x and y positions to those of 
					//the staircase we'll be coming down onto
					yPos = currentLevel->upStair.y;
					xPos = currentLevel->upStair.x;
					//Update our new tile so they know we're standing on them
					currentLevel->levelMap[yPos][xPos].character = this;
					//This action takes a turn
					passTurn = true;
				}
				break;

			case '<':
				//If we're standing on an up staircase, go up
				if(currentTile->terrain.symbol == '<'){
					//Go the previous level of the dungeon
					currentLevel = dungeon->level[currentLevel->levelNumber + - 1];
					//Remove ourselves from the tile we're standing on
					currentTile->character = NULL;
					//Update our x and y positions to those of 
					//the staircase we'll be coming down onto
					yPos = currentLevel->downStair.y;
					xPos = currentLevel->downStair.x;
					//Update our new tile so they know we're standing on them
					currentLevel->levelMap[yPos][xPos].character = this;
					//This action takes a turn
					passTurn = true;
				}
				break;
		}
		//If our direction tuple doesn't equal 0, 0, we're moving
		if(direction.y != 0 || direction.x != 0){
			//move in the given direction
			move(direction);
			//this takes a turn
			passTurn = true;
		}
		if (passTurn){
			//If we're taking a game turn, refresh our line of sight and regenerate
			getLineOfSight();
			healNaturally();
		}
	//Let the caller of the function know if we're passing the turn or not
	return passTurn;
	}

//The getLineOfSight function keeps track of what the character
//can and cannot see
//Returns: A vector of tile pointers, all of which fall within our character's LoS
vector<Tile*> Character::getLineOfSight(){
	//Set up our vector that we'll return
	vector<Tile*> lineOfSight;
	//Initialize a basic tile pointer
	Tile* tile;
	//Push the tile we're currently standing on 
	//into our line of sight
	lineOfSight.push_back(getTile());
	//If we're the player, some unique stuff happens
	if(isPlayer){
		//Clear the visibility markers on all tiles
		currentLevel->clearTileVisibility();
		//Clarify that we have explored the tile we're standing on,
		//and that it is visible
		Tile* selfTile = getTile();
		selfTile->visible = true;
		selfTile->explored = true;
	}
	//The distance of our line of sight is 8
	int distance = 8;

	//We're going to repeat the algorithm 8 times, 
	//one for each direction relative to the character.
	for(int direction = 0; direction < 8; direction++)
		//The goal of this algorithm is to determine every balanced 
		//digital line that can be drawn from the character to viable squares.
		//so we want to repeat Bresenham's line algorithm for every acceptable
		//slope of p/q and every acceptable starting point (eps).
		for(int q = 0; q <= distance; q++)
			for(int p = 0; p <= q; p++)
				for(int eps = 0; eps <= q; eps++){
					int y = 0;
					//The big EPS will be modified within our loop, but we will have
					//to keep track of the original value, so we leave the lower case one
					//to handle that
					int EPS = eps;

					//This is Bresenham's line algorithm
					for(int x = 1; x <= distance; x++){
						EPS += p;
						//Big X is going to be modified later in the algorithm,
						//But we need to keep small x intact as a counter.
						int X = x;
						if(EPS >= q){
							EPS -= q;
							//If the direction is 2, 3, 6 or 7 we're looking 
							//towards the top half of the map, so y decrements
							//as the eps grows greater than q
							if(direction & 2)
								y -= 1;
							else
								//Otherwise y increments since we're looking towards 
								//the bottom half of the map
								y += 1;
						}

						//If the direction is odd we're looking left instead of right.
						//So we invert x
						if(direction & 1)
							X = -X;

						//If the direction is 4, 5, 6 or 7, the y and x coordinate
						//are switched
						if(direction & 4)
							tile = &currentLevel->levelMap[X + yPos][y + xPos]; 
						else
							tile = &currentLevel->levelMap[y + yPos][X + xPos];
						//Once we've landed on our tile, we push it onto our line of sight
						lineOfSight.push_back(tile);

						//If we're the player, we can currently see the tile
						if(isPlayer){
							//so we mark it as visible so that it displays correctly
							tile->visible = true;
							//If we've never seen the tile before, we mark it as explored now
							if(!tile->explored)
								tile->explored = true;
						}
						//Otherwise we're a monster, and the only thing we care about is whether or not
						//the player is standing on a tile we can see
						else
							//If the player is on the current tile:
							if(tile->character != NULL && tile->character->isPlayer){
								//Create a monster pointer so we can access values that exist in the monster class
								Monster* thisMonster = this;
								//We can see the player
								thisMonster->canSeePlayer = true;
							}
						
						//If the current tile is a wall, we cannot see past it, so we break our current loop
						if(!tile->terrain.passable)
							break;
					}
			}
		//Return the tiles we can see
		return lineOfSight;
}

//Our findPath function finds a route to the target tile
//Parameters: 	A pointer to the tile we want a path to
//Returns: 		A stack of directions that will take our
//				character to that tile if they are followed
stack<Tuple> Monster::findPath(Tile* target){
	//Clear the pathvalues from every tile
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
	//adjTiles is going to be emptied and filled quite a lot
	//It'll be used to store the tiles adjacent to the current tile
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

//Our takeTurn function handles the monster's current turn
//Parameters: 	A character to attack if we can see them
void Monster::takeTurn(Character target){
	//We are regenerating if not at full health
	healNaturally();
	//Assume we can't see the player
	canSeePlayer = false;
	//Reevaluate if that's true
	getLineOfSight();
	//If we can see the player, find the quickest path to them
	if(canSeePlayer){
		stack <Tuple> foundPath = findPath(target.getTile());
		//Then follow that path
		move(foundPath.top());
		foundPath.pop();
	}
}
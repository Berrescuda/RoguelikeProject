//File name:mapObjects.cpp
//Author: Klemente Gilbert-Espada
//Description: This file determines how tiles, levels and the dungeon behave
//				(This includes the overall structure of the game)

//Our terrain initializer
//Takes a character, and sets our terrain to that character
//Parameters: Character to display as our terrain
Terrain::Terrain (char type){
	//Set our terrain's symbol to our input character
	symbol = type;
	//We know which symbols mean we can walk on this terrain
	if(symbol == '.' || symbol == '<' || symbol == '>')
		passable = true;
	//Otherwise the player can't move through us
	else passable = false;
}

//Our Tile initialization function 
//Parameters: A character (to be made into a terrain type), a y coordinate, 
//an x coordinate, and a level pointer
//Returns: A new tile
Tile::Tile(char c, int y, int x, Level* level){
		//Make sure our tile's level field points to the right level
		currentLevel = level;
		//Update our tile's x and y coordinates
		xPos = x;
		yPos = y;

		//Set our tile's terrain to whatever character
		//we've been passed
		terrain = Terrain(c);
		//Our tile starts without a character on it
		character = NULL;
		//Our tile starts unseen by the player
		visible = false;
		//And unexplored
		explored = false;

		//Depending on what character we've been given,
		//we put different objects on our tile
		switch(c){

			//Down staircase
			case '>':{
				//Let this level know where it's down staircase is
				currentLevel->downStair.y = yPos;
				currentLevel->downStair.x = xPos;
				break;
			}
			
			//Up staircase
			case '<':{
				//Let this level know where it's up staircase is
				currentLevel->upStair.y = yPos;
				currentLevel->upStair.x = xPos;
				break;
			}

			//Player
			case '@':{
				//Let everyone know where the player is
				player = Player(y, x);
				//update our tile's character field so that it points to the player
				character = &player;
				//The player is standing on a piece of floor
				terrain = Terrain('.');
				break;
			}
			
			//Space Goblin
			case 'g':{
				//Create a new SpaceGoblin, take a pointer to it
				character = new SpaceGoblin(y, x, level);
				//Push that pointer into our level's vector of monsters
				currentLevel->monsters.push_back(character);
				//The goblin is standing on a piece of floor
				terrain = Terrain('.');
				break;
			}

			//Potion
			case '%':{
				//Create a new potion, and take a pointer to it
				Potion* potionPointer = new Potion;
				//Push that pointer onto our vector of items
				items.push_back(potionPointer);
				//The potion is sitting on empty floor
				terrain = Terrain('.');
				break;
			} 
		}
}

//Our level initializer
//Parameters: 	A string representing a map of the level
//Returns: 		A level object
Level::Level(string map){
	int stringPos, y, x;
	stringPos = 0;

	//the first element in the vector was causing the
	//monster it referenced to act funny, so for now
	//this hack is here to keep that from happening:
	Monster* nullMonster;
	monsters.push_back(nullMonster);

	//the rest of the level initializer parses the string we're given and turns it into
	//a map
	//stringPos is used to keep track of where our cursor is in the string

	//while we're not at the end of the string
	for(y = 0; map[stringPos] != '\0'; y++){
		//Set up a new vector of tiles to push onto our map (rows)
		vector<Tile> row;
		//Go through the string until we hit a newline character,
		//which indicates the end of the row
		for(x = stringPos; map[x] != '\n'; x++)
			//If the space is empty we skip it
			if(map[x] != ' ')
				//Use the symbol on this point in the string
				//to initialize a tile, which we push onto our row
				row.push_back(Tile(map[x], y, (x - stringPos)/2, this));
		//Move forward a space
		stringPos = x + 1;
		//Push our row into our vector of rows (our current y coordinate)
		levelMap.push_back(row);
	}
}

//when we want to print our tile, generally we want to know what 
//its symbol is and what its color is.
//our printTile function returns those values
//returns: the symbol and color the tile shows on the screen
TileDisplayData Tile::printTile(void){
	//If the symbol is e, our space is empty and we put an empty tile
	if(terrain.symbol == 'e')
		return TileDisplayData{0, ' '};

	//If there's a character on our tile we return that character's symbol and color
	if(character != NULL)
		return TileDisplayData{character->color, character->symbol};

	//If there's no character but there is an item, we return the first item's symbol
	//and color
	if(!items.empty())
		return TileDisplayData{items[0]->color, items[0]->symbol};
	
	//Otherwise we return white and the symbol of our terrain
	return TileDisplayData{0, terrain.symbol};
	
}

//Our clearTileValues function goes through each tile 
//and removes the pathValue from it, allowing each 
//monster to use the findPath function unhindered
void Level::clearTileValues(){
	//For every row
	for(int y = 0; y < levelMap.size(); y++)
		//For every square
		for(int x = 0; x < levelMap[y].size(); x++)
			//Clean it up
			levelMap[y][x].pathValue = 0;
}

//Our clearTileVisibility functioni goes through every tile
//and marks it as not visible, so that the map doesn't display
//tiles we can't currently see as visible
void Level::clearTileVisibility(){
	//for each row
	for(int y = 0; y < levelMap.size(); y++)
		//for each square
		for(int x = 0; x < levelMap[y].size(); x++)
			//set visible to false
			levelMap[y][x].visible = false;
}

//listAdjacentTiles returns a list of tiles that are adjacent to the tile
	//calling the function
	//Parameters: 	A map of the level.
	//Returns: 		A list of adjacent tiles
	vector <Tile*> Tile::listAdjacentTiles(void){
		int x = xPos;
		int y = yPos;
		//Initialize our new vector
		vector <Tile*> adjacentTiles;

		//If there is a tile above us, add it to the list.
		if(y > 0)
			adjacentTiles.push_back(&currentLevel->levelMap[y - 1][x]);		//Up

		//If there is a tile below us, add it to the list.
		if(y < currentLevel->levelMap.size() - 1)
			adjacentTiles.push_back(&currentLevel->levelMap[y + 1][x]);		//Down

		//If there is a tile to our right, add it to the list.
		if (x < currentLevel->levelMap[y].size() - 1)
			adjacentTiles.push_back(&currentLevel->levelMap[y][x + 1]);		//Right

		//If there is a tile to our left, add it to the list.
		if (xPos > 0)
			adjacentTiles.push_back(&currentLevel->levelMap[y][x - 1]);		//Left

		//You get the idea
		if (y < currentLevel->levelMap.size() - 1 && x < currentLevel->levelMap[y].size() - 1)
			adjacentTiles.push_back(&currentLevel->levelMap[y + 1][x + 1]);	//DownRight
		
		if (y > 0 && x < currentLevel->levelMap[y].size() - 1)
			adjacentTiles.push_back(&currentLevel->levelMap[y -1][x + 1]);		//UpRight
		
		if (y > 0 && x > 0)
			adjacentTiles.push_back(&currentLevel->levelMap[y - 1][x - 1]);	//UpLeft

		if (y < currentLevel->levelMap.size() - 1 && x > 0)
			adjacentTiles.push_back(&currentLevel->levelMap[y + 1][x - 1]);	//DownLeft

		//Return our list
		return adjacentTiles;
	}


//Our Dungeon's processTurn function handles everything that happens during a game turn 
void Dungeon::processTurn(){
	//The input character
	char c;
	//While the player hasn't hit 'q'
	while(c != 'q'){
		//Display the map
		mainDisplay(*player.currentLevel);
		//Get a new keystroke
		c = getch();
		//Pass that keystroke to the player
		if (player.takeTurn(c)){
			//if the player took an in game turn, the rest of the turn proceeds
			//For every monster in our current level's vector of monsters 
			//(this should be updated to use an iterator)
			for(int i = 1; i < player.currentLevel->monsters.size(); i++){	
				//If the monster is alive (still exists)		
				if(player.currentLevel->monsters[i] != NULL)
					//The monster takes a turn
					player.currentLevel->monsters[i]->takeTurn(player);
			}

		}
	}
}


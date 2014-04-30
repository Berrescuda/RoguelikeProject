
Terrain::Terrain (char type){
	symbol = type;
	if(symbol == '.' || symbol == '<' || symbol == '>')
		passable = true;
	else passable = false;
}

//our initialization function takes a terrain type,
//when we initialize the tile we give it that type
Tile::Tile(char c, int y, int x, Level* level){
		currentLevel = level;
		xPos = x;
		yPos = y;
		terrain = Terrain(c);
		character = NullCharacter();
		switch(c){
			case '@':
				player = Player(y, x);
				character = player;
				terrain = Terrain('.');
				break;
			case 'g':
				character = SpaceGoblin(y, x, level);
				terrain = Terrain('.');
				break; 
		}
}

Level::Level(string map){
	level = 1;
	int stringPos, y, x;
	bool passable;
	stringPos = 0;
	for(y = 0; map[stringPos] != '\0'; y++){
		vector<Tile> newVector;
		for(x = stringPos; map[x] != '\n'; x++){
			if(map[x] != ' ')
				newVector.push_back(Tile(map[x], y, (x - stringPos)/2, this));
		}
		stringPos = x + 1;
		levelMap.push_back(newVector);
	}
}

//when we want to print our tile, generally we want to know what 
//its symbol is and what its color is.
//our printTile function returns those values
//paramaters: None
//returns: the symbol of the tile
TileDisplayData Tile::printTile(void){
	if(terrain.symbol == 'e')
		return TileDisplayData{0, ' '};
	if(character.symbol != ' ')
		return TileDisplayData{character.color, character.symbol};
	return TileDisplayData{0, terrain.symbol};
	
}

void Level::clearTileValues(){
	for(int y = 0; y < levelMap.size(); y++){
		for(int x = 0; x < levelMap[y].size(); x++)
			levelMap[y][x].pathValue = 0;
	}
}

//listAdjacentTiles returns a list of tiles that are adjacent to the tile
	//calling the function
	//Parameters: 	A map of the level.
	//Returns: 		A list of adjacent tiles
	list <Tile*> Tile::listAdjacentTiles(){
		int x = xPos;
		int y = yPos;
		//Initialize our new list
		list <Tile*> adjacentTiles;

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
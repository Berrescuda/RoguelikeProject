
Terrain::Terrain (char type){
	symbol = type;
	if(symbol == '.' || symbol == '<' || symbol == '>')
		passable = true;
	else passable = false;
}

//our initialization function takes a terrain type,
//when we initialize the tile we give it that type
Tile::Tile(char c, int y, int x, Level* level){
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


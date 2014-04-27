#include <iostream>

string rawMap = "e e e # # # # e e e e e e e e e e e e e e e e e # # # e e e e e e e e e e e e e e e \n"
				"e e e # . . # # # # # # # # # # # # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . . . . . . . . . . # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # # # # . # # # . # # # # # # # # . # # # # # # # # # # # # # # # # \n"
				"e e e # . # # . # e e # . # # # . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"e e e # . # # . # e e # . . . . . # # # # # # # # . # # # # # # # # # # # # # # # # \n"
				"e e e # . . > . # # # # % # # # # # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # # # # # # # @ . . # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e e e e e e e # . # # # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # # # e e e # . # e e e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # e e # g # e e e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . . # # # # . # # # # # # # # # # # # # # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . . . . . . . . . . . < . . . . . . . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # # # # . # # # . # # # # # # # # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # e e # . # # # . # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # e e # . . . . . # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . . . . # # # # . # # # # # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # # # # # # # . . . # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e e e e e e e # . # # # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e e e e e e e # # # e e e e e e e e e e e e # # # e e e e e e e e e e e e e e e \n";

Terrain::Terrain (char type){
	symbol = type;
	if(symbol == '.' || symbol == '<' || symbol == '>')
		passable = true;
	else passable = false;
}

//our initialization function takes a terrain type,
//when we initialize the tile we give it that type
Tile::Tile(char c, int y, int x){
		terrain = Terrain(c);
		character = NULL;
		switch(c){
			case '@':
				player = Player(y, x);
				character = &player;
				terrain = Terrain('.');
				break;
			case 'g':
				Character characterObject = SpaceGoblin(y, x);
				character = &characterObject;
				terrain = Terrain('.');
				break; 
		}
}

//when we want to print our tile, generally we want to know what 
//its symbol is and what its color is.
//our printTile function returns those values
//paramaters: None
//returns: the symbol of the tile
char Tile::printTile(void){
	if(terrain.symbol == 'e')
		return ' ';
	if(character != NULL)
		return character->symbol;
	return terrain.symbol;
	
}

Level::Level(string map){
	int stringPos, y, x;
	bool passable;
	stringPos = 0;
	for(y = 0; map[stringPos] != '\0'; y++){
		vector<Tile> newVector;
		for(x = stringPos; map[x] != '\n'; x++){
			if(map[x] != ' ')
				newVector.push_back(Tile(map[x], y, (x - stringPos)/2));
		}
		stringPos = x + 1;
		levelMap.push_back(newVector);
	}
}


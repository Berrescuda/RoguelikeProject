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

struct TerrainType{
	char symbol;
	bool passable;
	int color;
};

struct Terrain : TerrainType{
	Terrain(char);
};

Terrain::Terrain (char type){
	symbol = type;
	if(symbol == '.')
		passable = true;
	else passable = false;
}

struct Tile{
	bool visible;
	bool explored;

	//This value will be manipulated by our pathfinding algorithm
	int pathValue;
	TerrainType terrain;
	Character character;
	Tile(char, int, int);


	//when we want to print our tile, generally we want to know what 
	//its symbol is and what its color is.
	//our printTile function returns those values
	//paramaters: None
	//returns: the symbol of the tile
	char printTile(){
		if(terrain.symbol == 'e')
			return ' ';
		if(character.symbol != ' ')
			return character.symbol;
		return terrain.symbol;
		
	}

};

//our initialization function takes a terrain type,
//when we initialize the tile we give it that type
Tile::Tile(char c, int y, int x){
		terrain = Terrain(c);
		character = NullCharacter();
		switch(c){
			case '@':
				player = Player(y, x);
				character = player;
				terrain = Terrain('.');
				break;
			case 'g':
				character = SpaceGoblin(y, x);
				terrain = Terrain('.');
				break; 
		}

}

struct Level{
	vector<vector <Tile> > levelMap;
	list<Character> characters;

	Level(string map);

};

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

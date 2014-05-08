//File Name: 	mapObjects.h
//Author: 		Klemente Gilbert-Espada
//Description: 	Our mapObjects header defines all of the objects that exist
//				as part of the dungeon in the game

//Our terrainType class determines whether or not characters can pass through a tile,
//and what that tile displays when it's empty
struct TerrainType{
	//What we display when we're empty
	char symbol;
	//Whether or not we can be walked on
	bool passable;
};

//No idea why the hell this is still structured this way, will fix as soon as possible
struct Terrain : TerrainType{
	Terrain(char);
};

//This struct holds information about how a tile will be displayed to the screen
struct TileDisplayData{
	int color;
	char symbol;
};

//The Tile is one of the backbones of the program,
//They are a space in the dungeon, they hold the characters the items 
//and the shape of the dungeon itself
struct Tile{
	bool visible;
	bool explored;

	//This value will be manipulated by our pathfinding algorithm
	int pathValue;

	//Where our tile is located on the map
	int xPos;
	int yPos;

	//What type of terrain we have
	TerrainType terrain;
	//A pointer to the character standing on our tile
	Character* character;
	//A pointer to the level the tile exists on
	Level *currentLevel;

	//Our initializer
	Tile(char, int, int, Level*);
	//How we let the screen know how we want our tile to print
	TileDisplayData printTile(void);
	//Returns a vector of adjacent tiles
	vector <Tile*> listAdjacentTiles(void);

	//A vector of items on our tile
	vector <Item*> items;

};

//Our level struct represents a floor of the dungeon, 
//It has a map of what's on it comprised of tiles,
//It also keeps track of what monsters are running around on this level
struct Level{
	//Our map of tiles
	vector<vector <Tile> > levelMap;
	//All the monsters on the level
	vector<Monster*> monsters;
	//Which level of the dungeon we are
	int levelNumber;

	//Our initializer
	Level(string map);
	//Clear path values of every tile
	void clearTileValues();
	//Reset all tiles to not visible
	void clearTileVisibility();
	
	//The coordinates of our staircase that goes up
	Tuple upStair;
	//And the coordinates of the staircase that goes down
	Tuple downStair;

	//(We need these coordinates so we know where to put
	//the player when they enter our level)
};

//Our dungeon object is the object through which we essentially 
//find the rest of our game pieces, it keeps track of all of the levels,
//And runs the turn
struct Dungeon{
	//Our vector of levels in the dungeon
	vector<Level*> level;
	//processTurn manages who does what when
	//(It's also the core loop that runs the game)
	void processTurn();
};

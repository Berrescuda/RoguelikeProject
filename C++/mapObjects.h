struct TerrainType{
	char symbol;
	bool passable;
	int color;
};

struct Terrain : TerrainType{
	Terrain(char);
};

struct TileDisplayData{
	int color;
	char symbol;
};

struct Tile{
	bool visible;
	bool explored;

	//This value will be manipulated by our pathfinding algorithm
	int pathValue;
	int xPos;
	int yPos;

	TerrainType terrain;
	Character character;
	Level *currentLevel;

	Tile(char, int, int, Level*);
	TileDisplayData printTile(void);
	list <Tile*> listAdjacentTiles();

};

struct Level{
	vector<vector <Tile> > levelMap;
	list<Character> characters;
	int level;

	Level(string map);
	void clearTileValues();

};

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
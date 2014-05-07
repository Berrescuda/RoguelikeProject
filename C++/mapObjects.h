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
	const Character* character;
	Level *currentLevel;

	Tile(char, int, int, Level*);
	TileDisplayData printTile(void);
	vector <Tile*> listAdjacentTiles(void);
	vector <Item*> items;

};

struct Level{
	vector<vector <Tile> > levelMap;
	vector<Monster*> monsters;
	int levelNumber;

	Level(string map);
	void clearTileValues();
	void clearTileVisibility();
	void processTurn();
	Tuple upStair;
	Tuple downStair;
};

struct Dungeon{
	vector<Level*> level;
	void processTurn();
};

/*string rawMap = "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . @ . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # \n"
				"# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # \n";
*/
string rawMap = "e e e # # # # e e e e e e e e e e e e e e e e e # # # e e e e e e e e e e e e e e e \n"
				"e e e # . . # # # # # # # # # # # # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . . . . . . . . . . # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # # # # . # # # . # # # # # # # # . # # # # # # # # # # # # # # # # \n"
				"e e e # . # # . # e e # . # # # % % @ . . . . . . . . . . . . . . . . . . . . . . # \n"
				"e e e # . # # . # e e # . . . . . # # # # # # # # . # # # # # # # # # # # # # # # # \n"
				"e e e # . . > . # # # # . # # # # # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # # # # # # # . . . # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e e e e e e e # . # # # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # # # e e e # . # e e e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # e e # g # e e e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . . # # # # . # # # # # # # # # # # # # # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . . . . . . . . g . . < . . . . . . . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # # # # . # # # . # # # # # # # # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # e e # . # # # . # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . # # . # e e # . . . . . # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # . . . . # # # # . # # # # # e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e # # # # # # # . . . # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e e e e e e e # . # # # e e e e e e e e e e # . # e e e e e e e e e e e e e e e \n"
				"e e e e e e e e e # # # e e e e e e e e e e e e # # # e e e e e e e e e e e e e e e \n";
				/**/
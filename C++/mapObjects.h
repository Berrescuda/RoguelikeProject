struct TerrainType{
	char symbol;
	bool passable;
	int color;
};

struct Terrain : TerrainType{
	Terrain(char);
};



struct Tile{
	bool visible;
	bool explored;

	//This value will be manipulated by our pathfinding algorithm
	int pathValue;
	TerrainType terrain;
	Character* character;
	Tile(char, int, int);
	char printTile(void);

};

struct Level{
	vector<vector <Tile> > levelMap;
	list<Character> characters;

	Level(string map);

};
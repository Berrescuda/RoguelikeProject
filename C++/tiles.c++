#include <iostream>

struct TerrainType{
	char symbol;
	bool passable;
	int color;
};

struct Terrain : TerrainType{
	Terrain(char, bool);
};

Terrain::Terrain (char type, bool pass){
	symbol = type;
	passable = pass;
}

struct Tile{
	bool visible;
	bool explored;

	//This value will be manipulated by our pathfinding algorithm
	int pathValue;
	int yPos;
	int xPos;
	TerrainType terrain;
	Tile(Terrain, int, int);
};

	//our initialization function takes a terrain type,
	//when we initialize the tile we give it that type
	Tile::Tile(Terrain terr, int y, int x){
			terrain = terr;
			yPos = y;
			xPos = x;
	}

int main(){
	Terrain terrain('.', true);
	Tile tile(terrain, 4, 5);
	std::cout << tile.terrain.symbol << "\n";
}
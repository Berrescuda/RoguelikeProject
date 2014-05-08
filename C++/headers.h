//Title: headers.h
//Author: Klemente Gilbert-Espada
//Description: This is just where we're forward declaring some structs and
//importing our own header files

//This struct is just here to simplify some stuff later
struct Tuple{
	int y;
	int x;
};

//Forward declarations
struct Character;
struct Level;
struct Tile;
struct Dungeon;
struct Item;
//Import our own headers
#include "characters.h"
#include "mapObjects.h"
#include "items.h"
#include "kcurses.h"

//Forward declare our log function
void log(string);
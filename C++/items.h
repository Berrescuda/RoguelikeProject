
//File name: items.h
//Author: Klemente Gilbert-Espada										
//Description: This is where items will be defined in the game
//Right now we only have the one item subclass, but future items will also
//go here.
//Items can be picked up, but they sit on the floor usually, displaying a symbol
struct Item{
	//What the item will display on the map
	char symbol;
	//Color of the item
	int color;
	//Name of the item
	string name;
	//Right now characters can drink any item.
	//(Look, right now all items are potions, I'll fix it later)
	void drink(Character*);

};

//Potions heal their user by five hp
struct Potion: Item{
	Potion();
	
};

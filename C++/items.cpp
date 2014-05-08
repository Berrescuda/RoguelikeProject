
//File name: items.cpp
//Author: Klemente Gilbert-Espada										
//Description: This is where items functions will be defined in the game

Potion::Potion(){
		symbol = '%';
		color = COLOR_BLUE;
		name = "potion";
}

//The drink function uses the potion and heals the user
//Parameters: 	character using the potion
//Returns: 		Nothing
void Item::drink(Character* character){
	//Heal the character
	character->currentHp += 5;
	//But cap it out at the maxHp
	if (character->currentHp > character->maxHp)
		character->currentHp = character->maxHp;
}
##########################################################################
#File name: items.py
#Author: Klemente Gilbert-Espada										
#Description: This is where items will be defined in the game
#Right now we only have the one item subclass, but future items will also
#go here.
##########################################################################

import curses

#Items can be picked up, but they sit on the floor usually, displaying a symbol
class Item:
	#What the item will display on the map
	symbol = "%"
	#Color of the item
	color = curses.COLOR_BLUE
	#Name of the item
	name = "item"

#Potions heal their user by five hp
class Potion(Item):
	#change the name
	name = "potion"
	#The drink function uses the potion and heals the user
	#Parameters: 	character using the potion
	#Returns: 		Nothing
	def drink(self, character):
		#Heal the character
		character.currentHp += 5
		#But cap it out at the maxHp
		if character.currentHp > character.maxHp:
			character.currentHp = character.maxHp
		#Use up the potion
		character.inventory.remove(self)

class Flask(Item):
	#change the name
	name = "flask"
	#change the symbol
	symbol = "&"
	#The drink function uses the flask and increases the user's magicpoints
	#Parameters:	character using the potion
	#Returns:		Nothing
	def drink(self, character):
		#Boost the current magicpoints
		character.currentMp += 5
		#Make sure the currentMp doesnt exceed the cap
		if character.currentMp > character.maxMp:
			character.currentMp = character.maxMp
		#Use up the potion
		character.inventory.remove(self)
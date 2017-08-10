List of directories:


- EducRace  contains units and data for the game.
 An executable for Windows can be found at http://www.lulutech.fr/EducRace/EducRace.7z


- OALSoundManager  contains units for audio. It use OpenAL library.


- OGLScene  contains units for OpenGL scene. It use TOpenGLControl.
I start to write my framework 3/4 years ago slowly. It use OpenGL through TOpenGLControl. I got inspired by ZENGL written by Andrey Kemka, so this is not modern OpenGL programming. I use "glBegin" / glEnd" structure to draw graphics.


- TileMapDesigner ( sub-directory in OGLScene folder ) contains units for tools to create tiled map.
  I have written it to make the differents stages for the game Educ Race.
           - allow multiple tileset
           - user's event value on each tile
           - ground type
           - change the color of the 'holes' in your map
           - can draw a second map on top or back the working map to 'synchronize' them




all files compile and run well on windows 7 and 10, but my work remains experimental! :)

lulu
contact@lulutech.fr
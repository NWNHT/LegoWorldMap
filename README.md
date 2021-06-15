# LegoWorldMap
Extract Lego World Map set 31203 layout from product image and allow for generation of patterns.

LegoWorldMapExtraction: Land vs. Water data extracted from included image mono.jpg then minor manual additions are made to clean up the west coast of the US.

LegoWorldMapPatterns: Generate patterns based on the extracted data
- Adjacent4: Assigns colour to non-land studs that are adjacent to land studs in the four cardinal directions
- Adjacent8: Assigns colour to non-land studs that are adjacent to land studs in the four cardinal directions and a single stud diagonally
- ShadowEast: Assigns colour to non-land studs directly to the east of land studs.

Completed map dataframes are stored in the CompletedMaps directory

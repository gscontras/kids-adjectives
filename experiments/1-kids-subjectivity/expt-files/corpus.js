// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"blonde", "Class":"color"},
		{"Predicate":"senior", "Class":"age"},
		{"Predicate":"elderly", "Class":"age"},
		{"Predicate":"major", "Class":"value"},
		{"Predicate":"useful", "Class":"value"},
		{"Predicate":"simple", "Class":"value"},
		{"Predicate":"cheap", "Class":"value"},
		{"Predicate":"comfortable", "Class":"value"},
		{"Predicate":"fine", "Class":"value"},
		{"Predicate":"expensive", "Class":"value"},
		{"Predicate":"gross", "Class":"value"},
		{"Predicate":"positive", "Class":"value"},
		{"Predicate":"negative", "Class":"value"},
		{"Predicate":"nice", "Class":"value"},
		{"Predicate":"best", "Class":"value"},
		{"Predicate":"easy", "Class":"value"},
		{"Predicate":"basic", "Class":"value"},
		{"Predicate":"minor", "Class":"value"},
		{"Predicate":"difficult", "Class":"value"},
		{"Predicate":"pure", "Class":"value"},
		{"Predicate":"normal", "Class":"value"},
		{"Predicate":"important", "Class":"value"},
		{"Predicate":"significant", "Class":"value"},
		{"Predicate":"excellent", "Class":"value"},
		{"Predicate":"necessary", "Class":"value"},
		{"Predicate":"boring", "Class":"value"},
		{"Predicate":"average", "Class":"value"},
		{"Predicate":"practical", "Class":"value"},
		{"Predicate":"common", "Class":"value"},
		{"Predicate":"enormous", "Class":"dimension"},
		{"Predicate":"massive", "Class":"dimension"},
		{"Predicate":"deep", "Class":"dimension"},
		{"Predicate":"solid", "Class":"physical"},
		{"Predicate":"far", "Class":"location"},
		{"Predicate":"outer", "Class":"location"},
		{"Predicate":"northern", "Class":"location"},
		{"Predicate":"regional", "Class":"location"},
		{"Predicate":"internal", "Class":"location"},
		{"Predicate":"inner", "Class":"location"},
		{"Predicate":"north", "Class":"location"},
		{"Predicate":"local", "Class":"location"},
		{"Predicate":"international", "Class":"nationality"},
		{"Predicate":"British", "Class":"nationality"},
		{"Predicate":"european", "Class":"nationality"},
		{"Predicate":"funny", "Class":"human"},
		{"Predicate":"current", "Class":"temporal"},
		{"Predicate":"future", "Class":"temporal"},
		{"Predicate":"recent", "Class":"temporal"},
		{"Predicate":"parliamentary", "Class":"other"},
		{"Predicate":"industrial", "Class":"other"},
		{"Predicate":"financial", "Class":"other"},
		{"Predicate":"voluntary", "Class":"other"},
		{"Predicate":"civil", "Class":"other"},
		{"Predicate":"commercial", "Class":"other"},
		{"Predicate":"political", "Class":"other"},
		{"Predicate":"legal", "Class":"other"},
		{"Predicate":"general", "Class":"other"},
		{"Predicate":"specific", "Class":"other"},
		{"Predicate":"environmental", "Class":"other"},
		{"Predicate":"statutory", "Class":"other"},
		{"Predicate":"individual", "Class":"other"},
		{"Predicate":"economic", "Class":"other"},
		{"Predicate":"military", "Class":"other"},
		{"Predicate":"technical", "Class":"other"},
		{"Predicate":"agricultural", "Class":"other"},
		{"Predicate":"democratic", "Class":"other"},
		{"Predicate":"public", "Class":"other"},
		{"Predicate":"complete", "Class":"other"}
]);

var nouns = [
		{"Noun":"apple", "NounClass":"food"},
		{"Noun":"banana", "NounClass":"food"},
		{"Noun":"carrot", "NounClass":"food"},
		{"Noun":"cheese", "NounClass":"food"},
		{"Noun":"tomato", "NounClass":"food"},								
		{"Noun":"chair", "NounClass":"furniture"},								
		{"Noun":"couch", "NounClass":"furniture"},								
		{"Noun":"fan", "NounClass":"furniture"},								
		{"Noun":"TV", "NounClass":"furniture"},								
		{"Noun":"desk", "NounClass":"furniture"}								
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	for (var i=0; i<adjectives.length; i++) {
		// noun = _.sample(nouns);
		stims.push(
			{
				"Predicate":adjectives[i].Predicate,
				"Class":adjectives[i].Class,				
				// "Noun":noun.Noun,
				// "NounClass":noun.NounClass
			}
			);
		}
		
	return stims;
	
}
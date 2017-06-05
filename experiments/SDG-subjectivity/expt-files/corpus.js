// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"red", "Class":"color"},
		{"Predicate":"yellow", "Class":"color"},
		{"Predicate":"green", "Class":"color"},
		{"Predicate":"blue", "Class":"color"},
		{"Predicate":"purple", "Class":"color"},
		{"Predicate":"brown", "Class":"color"},											
		{"Predicate":"big", "Class":"size"},
		{"Predicate":"small", "Class":"size"},					
		{"Predicate":"huge", "Class":"size"},					
		{"Predicate":"tiny", "Class":"size"},					
		{"Predicate":"short", "Class":"size"},					
		{"Predicate":"long", "Class":"size"},							
		{"Predicate":"wooden", "Class":"material"},
		{"Predicate":"plastic", "Class":"material"},
		{"Predicate":"metal", "Class":"material"},
		{"Predicate":"smooth", "Class":"texture"},
		{"Predicate":"hard", "Class":"texture"},
		{"Predicate":"soft", "Class":"texture"},
		{"Predicate":"old", "Class":"age"},
		{"Predicate":"new", "Class":"age"},
		{"Predicate":"rotten", "Class":"age"},
		{"Predicate":"fresh", "Class":"age"},
		{"Predicate":"good", "Class":"quality"},
		{"Predicate":"bad", "Class":"quality"},
		{"Predicate":"round", "Class":"shape"},						
		{"Predicate":"square", "Class":"shape"}
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
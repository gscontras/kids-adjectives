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
		{"Predicate":"gold", "Class":"color"},	
		{"Predicate":"golden", "Class":"color"},	
		{"Predicate":"black", "Class":"color"},	
		{"Predicate":"orange", "Class":"color"},
		{"Predicate":"silver", "Class":"color"},
		{"Predicate":"tawny", "Class":"color"},	
		{"Predicate":"white", "Class":"color"},	
		{"Predicate":"grey", "Class":"color"},		
		{"Predicate":"pink", "Class":"color"},	
		{"Predicate":"gray", "Class":"color"},			
		{"Predicate":"big", "Class":"size"},
		{"Predicate":"small", "Class":"size"},					
		{"Predicate":"huge", "Class":"size"},					
		{"Predicate":"tiny", "Class":"size"},					
		{"Predicate":"short", "Class":"size"},					
		{"Predicate":"long", "Class":"size"},	
		{"Predicate":"wee", "Class":"size"},	
		{"Predicate":"weensie", "Class":"size"},
		{"Predicate":"fat", "Class":"size"},	
		{"Predicate":"eensie", "Class":"size"},
		{"Predicate":"crust", "Class":"size"},	
		{"Predicate":"teeny", "Class":"size"},	
		{"Predicate":"podgy", "Class":"size"},
		{"Predicate":"itty", "Class":"size"},
		{"Predicate":"spin", "Class":"size"},	
		{"Predicate":"puff", "Class":"size"},
		{"Predicate":"chubby", "Class":"size"},
		{"Predicate":"thick", "Class":"size"},
		{"Predicate":"little", "Class":"size"},
		{"Predicate":"flat", "Class":"size"},
		{"Predicate":"wide", "Class":"size"},
		{"Predicate":"fleece", "Class":"size"},
		{"Predicate":"thin", "Class":"size"},
		{"Predicate":"huge", "Class":"size"},
		{"Predicate":"itsy", "Class":"size"},
		{"Predicate":"tubby", "Class":"size"},
		{"Predicate":"bitsy", "Class":"size"},
		{"Predicate":"tall", "Class":"size"},
		{"Predicate":"weeny", "Class":"size"},
		{"Predicate":"wooden", "Class":"material"},
		{"Predicate":"plastic", "Class":"material"},
		{"Predicate":"wool", "Class":"material"},
		{"Predicate":"smooth", "Class":"texture"},
		{"Predicate":"hard", "Class":"texture"},
		{"Predicate":"soft", "Class":"texture"},
		{"Predicate":"rough", "Class":"texture"},
		{"Predicate":"damp", "Class":"texture"},
		{"Predicate":"sharp", "Class":"texture"},
		{"Predicate":"right", "Class":"texture"},
		{"Predicate":"dry", "Class":"texture"},
		{"Predicate":"wet", "Class":"texture"},
		{"Predicate":"old", "Class":"age"},
		{"Predicate":"new", "Class":"age"},
		{"Predicate":"rotten", "Class":"age"},
		{"Predicate":"fresh", "Class":"age"},
		{"Predicate":"young", "Class":"age"},
		{"Predicate":"ancient", "Class":"age"},
		{"Predicate":"ole", "Class":"age"},
		{"Predicate":"dead", "Class":"age"},
		{"Predicate":"wobble", "Class":"age"},
		{"Predicate":"good", "Class":"quality"},
		{"Predicate":"bad", "Class":"quality"},
		{"Predicate":"awful", "Class":"quality"},
		{"Predicate":"nasty", "Class":"quality"},
		{"Predicate":"horrid", "Class":"quality"},
		{"Predicate":"thirst", "Class":"quality"},
		{"Predicate":"great", "Class":"quality"},
		{"Predicate":"brilliant", "Class":"quality"},
		{"Predicate":"grand", "Class":"quality"},
		{"Predicate":"nice", "Class":"quality"},
		{"Predicate":"poor", "Class":"quality"},
		{"Predicate":"okay", "Class":"quality"},
		{"Predicate":"alright", "Class":"quality"},
		{"Predicate":"crummy", "Class":"quality"},
		{"Predicate":"ordinary", "Class":"quality"},
		{"Predicate":"round", "Class":"shape"},						
		{"Predicate":"square", "Class":"shape"},
		{"Predicate":"oval", "Class":"shape"},
		{"Predicate":"misshapen", "Class":"shape"}
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
console.time('aoc6');
var input = require('./aoc6.input.json');
var letterScores = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
var letterMap = {a: 0, b: 1,c: 2,d: 3,e: 4,f: 5,g: 6,h: 7,i: 8,j: 9,k: 10,l: 11,m: 12,n: 13,o: 14,p: 15,q: 16,r: 17,s: 18,t: 19,u: 20,v: 21,w: 22,x: 23,y: 24, z: 25};
var letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"];

var occurrences = [
	[].concat(letterScores),
	[].concat(letterScores),
	[].concat(letterScores),
	[].concat(letterScores),
	[].concat(letterScores),
	[].concat(letterScores),
	[].concat(letterScores),
	[].concat(letterScores)
];

var input2 = input.map(function(str) {
	return str.split("");
});

input2.forEach(function(arr) {
	arr.forEach(function(char, idx) {
		occurrences[idx][letterMap[char]]++;
	});
});

var mostOccurrences = occurrences.map(function(arr) {
	return arr.reduce(function(acc, occ, index) {
		if (occ > acc.occurrences) {
			return { idx: index, occurrences: occ };
		}
		return acc;
	}, {idx: 0, occurrences: 0});
});

var letters = mostOccurrences.map(function(occurrence) {
	return letters[occurrence.idx];
});

console.log(letters.join(""));
console.timeEnd('aoc6');

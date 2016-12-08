console.time('aoe6-2');
var input = require('./aoc6.input.json');
var flippedInput = [[], [], [], [], [], [], [], []];

input.forEach(function(str) {
	str.split("").forEach(function(char, idx) {
		flippedInput[idx].push(char);
	});
});

var sortedInput = flippedInput.map(arr => arr.sort());

var mostOccurringLetters = sortedInput.map(function(arr) {
	var i = 0;
	var res = { char: '', occurrences: 0 };

	do {
		var lastIndex = arr.lastIndexOf(arr[i]);
		var occurrences = lastIndex - i + 1;
		var res = occurrences > res.occurrences ? { char: arr[i], occurrences: occurrences} : res;
		i = i + occurrences;
	} while (i < arr.length);

	return res.char;
});

console.log(mostOccurringLetters.join(""));
console.timeEnd('aoe6-2');

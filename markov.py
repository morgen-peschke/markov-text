#!/usr/bin/env python
#################################################################################
#
# Author: Morgen Peschke
# Date:   Dec 15th, 2013
#
# Description: This is a random text generator that uses the Markov Chaining
#              technique to create text with passing similarity to valid English
#              text. This is also an exploration of the idea of graceful
#              degradation, by attempting to provide a match of the requested
#              order, but returning matches of lower orders when that is not
#              possible. In this manner the algorithm attempts to recover from
#              an otherwise fatal dead end chain.
#
# Usage: ./markov.py <file 1> <file 2> ... <file n>
#
#        The script will read in and will attempt to create a string of random
#        words of the specified length and order. Customizing the default
#        behavior is handled in the constructor. For more control the script
#        can be imported an used as a library.
#
# Acknowledgments: Although the code has massively diverged from the sample
#                  code, and has been basically been rebuild from scratch, the
#                  genesis of this script came from the sample code provided at
#                  http://xkcd.com/markov.py.txt as part of the XKCD "What if?"
#                  titled "Phone Keypad" [http://what-if.xkcd.com/75/].
#
#                  The article provided the impetus for the creation of this
#                  script, and as such, I would be remiss to neglect giving
#                  credit where it is due.
#
import random
import sys
import copy

class markov_text_generator:

    def __init__ (self, **kwargs):

        self.min_sentence_length = kwargs.get ('length') or 10
        self.order               = kwargs.get ('order')  or 3
        self.source_file         = kwargs.get ('file')

        self.dictionaries = None

        if self.order < 2: self.order = 2

        if self.source_file:
            self.gen_dictionaries ()

    # generator to read file as words
    @staticmethod
    def read_words (fileobj):
        for line in fileobj:
            for word in line.split():
                yield word

    # set input file
    def set_file (self, file_path):
        self.source_file = file_path
        self.dictionaries = None
        return self

    # clears out the old dictionaries and sets up for a new generation run
    def _clear_dictionaries (self):

        self.dictionaries = []

        # Load the dictionaries list with an empty dict for each order 2 and
        # above
        for i in range (1, self.order):
            self.dictionaries.append ({})

        return self

    # generate the dictionaries
    def _gen_dictionaries (self):
        with open (self.source_file) as fh:

            # set up required to read the file word-wise
            words = markov_text_generator.read_words (fh)

            self._clear_dictionaries()

            # build a list of the same length as the order + 1, with 'None' as
            # the 0 index value
            tmp = [None]
            for i in range (self.order):
                tmp.append (words.next())

            # read until the end of the file and add each set to the appropriate
            # dictionary
            for w in words:
                tmp = tmp[1:] + [ w ]

                # build and add the sequences
                for i in range (1, self.order):
                    key = tuple (tmp[:i + 1])

                    if not self.dictionaries[i - 2].has_key (key):
                        self.dictionaries[i - 2][key] = []

                    self.dictionaries[i - 2][key].append(tmp[i + 1])

            # finish processing the last few words
            while len (tmp) >= self.order:
                tmp = tmp[1:]

                # build and add the sequences
                for i in range (1, len (tmp) - 1):
                    key = tuple (tmp[:i + 1])

                    if not self.dictionaries[i - 2].has_key (key):
                        self.dictionaries[i - 2][key] = []

                    self.dictionaries[i - 2][key].append (tmp[i + 1])

        # remove empty orders
        self.dictionaries = [ x for x in self.dictionaries if len (x) ]
        return self

    # Generate a candidate sentence
    def _make_sentence (self):

        # Cache the highest order
        max_order = len (self.dictionaries) - 1

        # Set the initial candidate to a random key in the highest order
        candidate = list (random.choice (self.dictionaries[-1].keys()))

        # Start looking at the highest order
        cur_order = max_order

        # Generate the longest possible sentence
        while True:
            # The length of the current key is needed to generate a valid key
            # from the current candidate sentence
            key_len = len(self.dictionaries[cur_order].keys()[0])

            try:
                options = self.dictionaries[cur_order][tuple(candidate[-key_len:])]

            except KeyError:
                return candidate

            options.sort (key = lambda x: len(x) + random.random () * 5.0,
                          reverse = True)

            # Try for the best possible option
            found = False
            for o in options:

                # A best option:
                # - is part of another key (not a dead end, higher order is better)
                # - is not already in the candidate sentence (avoid endless
                # loops)
                new_key = tuple(candidate[-(key_len - 1):] + [o])

                #print " new_key: %s" % (str(new_key))

                if (self.dictionaries[cur_order].has_key (new_key)
                    and o not in candidate):

                    candidate.append(o)
                    cur_order = max_order

                    found = True
                    break

            # If a best option is not found and we've bottomed out the potential
            # Markov orders, just add anything and terminate the chain
            if not found and cur_order == 0:
                candidate.append (random.choice (options))
                return candidate

            # If a best option is not found at this level, try at a lower level
            elif not found:
                cur_order -= 1

        return candidate

    # Attempt to generate a sentence of a given length by generating random length
    # sentences until one of the appropriate length pops out
    def generate (self):

        if self.dictionaries == None:
            self._gen_dictionaries()

        while (True):
            candidate = self._make_sentence()

            if len(candidate) >= self.min_sentence_length:
                return ' '.join(candidate)


if __name__ == '__main__':
    gen = markov_text_generator ()

    for i in sys.argv[1:]:
        print gen.set_file (i).generate()

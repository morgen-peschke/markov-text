markov-text
===========

This is a random text generator that uses the Markov Chaining
technique to create text with passing similarity to valid English
text. This is also an exploration of the idea of graceful degradation,
by attempting to provide a match of the requested order, but returning
matches of lower orders when that is not possible. In this manner the
algorithm attempts to recover from an otherwise fatal dead end chain.

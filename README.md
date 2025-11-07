# Natural Language Interface Based on Lambek Calculus

**Course:** CSE301 – Functional Programming  
**Institution:** École Polytechnique  
**Contributors:** Youssef Naji, Myriam Dardour, Karl Khalaf, Elisa Marlow
**Git Repository:** https://github.com/elisamarlow/Functional-Programming-Final-Project

---

## Project Overview

This project implements a **natural language interface** in **Haskell** based on **Joachim Lambek’s calculus of syntactic types (1958)**.  
The goal is to connect **grammar**, **logic**, and **compositional semantics**, transforming English sentences into **lambda expressions** that can be **executed as Python code** to evaluate their truth or meaning.

It combines ideas from formal linguistics, lambda calculus, and functional programming.

---

## Step-by-Step Summary of What Was Done

### 1. Understanding Lambek Calculus
We started by studying the rules of **Lambek’s sequent calculus**, which defines how types such as `np`, `s`, and `adj` combine grammatically.  
The two mutually recursive functions:
- `dec_rinv`  
- `dec_lfoc`  
were implemented to decide whether a sequence of word types can form a valid sentence (`Γ ⊢ s`).

These functions apply the inference rules for `/` and `\` (right and left division) and check grammaticality.

---

### 2. From Grammaticality to Lambda Expressions
Once grammaticality worked, the same algorithm was extended to **build lambda expressions** instead of just returning `True` or `False`.

We wrote:
- `der_rinv` — builds the expression corresponding to right-inversion rules.  
- `der_lfoc` — constructs expressions while focusing on one type and applying the appropriate arguments.

This step allowed us to compute the **logical form** of a sentence such as:


Awen likes Butor
→ Likes_2 Awen_1 Butor_3



---

### 3. Adding Meaning with Python Semantics
Each word in the lexicon (`mini.elx`) was given not only a type but also a **Python semantic value**.  
For example:

Likes : np \ (s / np) = lambda x : lambda y : (x,y) in likes.
Happy : adj            = lambda x : x in happy.

A Python interpreter was launched from Haskell (`PySupport.hs`) to evaluate the resulting expressions in a database of facts (`miniworld.py`).

This made it possible to obtain **True/False** results for declarative sentences:

> Awen likes Butor
True


---

### 4. Handling Coordination and Ambiguity

We added polymorphic entries for **“and”**, allowing it to coordinate:

* Sentences (`s \ (s / s)`)
* Adjectives (`adj \ (adj / adj)`)
* Noun phrases (`np \ (np / np)`)

This made sentences like:

> Butor is happy and confused
True
> Butor likes Céleste and Awen
False


work correctly, and allowed ambiguous structures such as:


> Everyone likes someone
That sentence is ambiguous


producing two distinct derivations.

---

### 5. Extending the Grammar with Negation and “Who”

We extended the lexicon to handle **negation** and **questions**.

* **Negation**

  Not : adj / adj = lambda p : lambda x : not p(x).

> Butor is not happy
 False

* **Who-Questions**

  Who : s / (np \ s) = lambda p : [x for x in people if p(x)].


  → “Who likes Butor” → `[‘Awen’, ‘Butor’]`.

We also handled the special case where no one satisfies the condition:

> Who is not happy
Answer: No one

---

### 6. Interactive Interface

The main program (`ChatLLaMbda.hs`) implements a **read–eval–print loop (REPL)** that:

1. Reads a sentence from the user,
2. Parses it through the Lambek algorithm,
3. Builds its lambda term,
4. Translates it to Python,
5. Runs it in a Python interpreter,
6. Prints the result or list of entities.

---

                                                                                                       

##  How to Run

1. Compile:

   ```bash
   ghc -o ChatLLaMbda ChatLLaMbda.hs
   ```

2. Run:

   ```bash
   ./ChatLLaMbda


3. Provide:


   Lexicon file: mini.elx
   Facts about the world: miniworld.py
   Starting chat...


4. Then test sentences 


---


import pyttsx3
from docx import Document
import speech_recognition as sr 
from TextToSpeech import getQuestion
from SpeechToText import getAnswer

"""
    1. Speak first question
    2. Wait for ans
    3. Store ans in dict
    4. Narrate next question.....
"""

curr_question = 1
commandCode = 'command'

doc = Document("paper.docx")

answers = {}
questions = {}
paper = []
commands = {
    "help": 1,
    "skip": 1,
    "previous" : 1,
    "reread": 1,
    "goto": 1,
    "back space": 1,
    "submit": 1,
    "confirm": 1,
    "mark review": 1,
    "next": 1,
    "repeat question": 1,
    "time left": 1,
}

engine = pyttsx3.init()
rate = engine.getProperty('rate')
engine.setProperty('rate', rate-50)

for para in doc.paragraphs:
    paper.append(para.text)

for i in range(len(paper)):
    try:
        x = paper[i].split('.')[0]
        if x[0] == 'Q':
            questions[int(x[1:])] = i
            optNo = 0
        else:
            optNo += 1
            paper[i] = str(optNo) + '. ' + paper[i]

    except: pass

"""for q in sorted(questions.keys()):
    getQuestion(q,paper,questions)
    text = getAnswer(q)
    answers[q] = text

"""
while(1):
    getQuestion(curr_question,paper,questions)
    text = getAnswer(curr_question)
    if text.find(commandCode) != -1:
        print("Command : " + text)
        commandSplit = text.split(commandCode)
        #pos = commandSplit.index(commandCode)
        command = " ".join(commandSplit[1:]).strip()
        if command in commands.keys():
            print(command + " in keys")
            
        print(command)
    else:
        answers[curr_question] = text
        curr_question += 1
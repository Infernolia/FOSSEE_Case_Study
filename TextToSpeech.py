from gtts import gTTS
from os import system

def getQuestion(q, paper, questions):
    question = []
    try:
        for i in range(questions[q], questions[q+1]):
            question.append(paper[i])
    except KeyError:
        for i in range(questions[q], len(paper)):
            question.append(paper[i])
    audio = gTTS('.\n\n'.join(question), 'en', False)
    audio.save('temp.mp3')
    system('mpg321 temp.mp3')

from docx import Document
doc = Document("paper.docx")

questions = {}
paper = []

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

def showPaper():
    for line in paper:
        print(line)        

def showQuestion(q):
    try:
        for i in range(questions[q], questions[q+1]):
            print(paper[i])
    except KeyError:
        for i in range(questions[q], len(paper)):
            print(paper[i])
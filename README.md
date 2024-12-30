# group_chat_wrapped

## What is this?

This is a script that analyses Facebook Messenger chat data saved in the 'group chat 2024' folder or any alternative folder, using two distinct analyses:
- Network analysis - using the PageRank algorithm and edge weights defined by the number or mentions and reacts given by members of the group chat to other
  members of the group chat.
- Sentiment analysis - using the NRC Emotion Lexicon (Mohammad & Turny, 2011).

This data is spit out in the outputs folder as a graph of the 'network' in addition to an Excel of detailed results. These can be presented in visualisations such as the 'Example.jpg' provided.

## Steps to run:

1. Download your information in https://accountscenter.facebook.com/info_and_permissions 
    - Only requires messages information, in HTML format.

2. Find the chat of interest in your downloaded Facebook information information your_facebook_activity\messages\inbox\[CHAT_NAME]
    - Then save the HTML file 'message_[NUM]' etc. into a folder in the same directory as the R Project (e.g. I've been using the 'group chat 2024' folder).

3. Fill in the necessary manual info at the top of the R script, including noting the folder to analyse data within above and also a concordance of nicknames.

4. Run the script and look in the Outputs folder to enjoy the chaos of the results you have just uncovered!

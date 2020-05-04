current_list = 1;
var order = 1;

var all_stimuli = [{"item":1,"block":1,"list":1,"condition":"CNPC","Context":"The teacher believes the claim that the boy found a box of diamonds.","Target":"What does the teacher believe the claim that the boy found?","presented_context":"Context: The teacher believes the claim that the boy found a box of diamonds.","presented_target":"<b>What does the teacher believe the claim that the boy found?<\/b>"},{"item":1,"block":1,"list":2,"condition":"WH","Context":"The teacher wonders whether the boy found a box of diamonds.","Target":"What does the teacher wonder whether the boy found?","presented_context":"Context: The teacher wonders whether the boy found a box of diamonds.","presented_target":"<b>What does the teacher wonder whether the boy found?<\/b>"},{"item":1,"block":1,"list":3,"condition":"SUBJ","Context":"The teacher thinks that a box of diamonds was found by the boy.","Target":"What does the teacher think that a box of was found by the boy?","presented_context":"Context: The teacher thinks that a box of diamonds was found by the boy.","presented_target":"<b>What does the teacher think that a box of was found by the boy?<\/b>"},{"item":2,"block":1,"list":2,"condition":"CNPC","Context":"The professor believes the claim that the student spilled a bottle of water.","Target":"What does the professor believe the claim that the student spilled?","presented_context":"Context: The professor believes the claim that the student spilled a bottle of water.","presented_target":"<b>What does the professor believe the claim that the student spilled?<\/b>"},{"item":2,"block":1,"list":3,"condition":"WH","Context":"The professor wonders whether the student spilled a bottle of water.","Target":"What does the professor wonder whether the student spilled?","presented_context":"Context: The professor wonders whether the student spilled a bottle of water.","presented_target":"<b>What does the professor wonder whether the student spilled?<\/b>"},{"item":2,"block":1,"list":1,"condition":"SUBJ","Context":"The professor thinks that a bottle of water was spilled by the student.","Target":"What does the professor think that a bottle of was spilled by the student?","presented_context":"Context: The professor thinks that a bottle of water was spilled by the student.","presented_target":"<b>What does the professor think that a bottle of was spilled by the student?<\/b>"},{"item":3,"block":1,"list":3,"condition":"CNPC","Context":"The investigator acknowledges the possibility that the robbers deleted a segment of the surveillance video.","Target":"What does the investigator acknowledge the possibility that the robbers deleted?","presented_context":"Context: The investigator acknowledges the possibility that the robbers deleted a segment of the surveillance video.","presented_target":"<b>What does the investigator acknowledge the possibility that the robbers deleted?<\/b>"},{"item":3,"block":1,"list":1,"condition":"WH","Context":"The investigator wonders whether the robbers deleted a segment of the surveillance video.","Target":"What does the investigator wonder whether the robbers deleted?","presented_context":"Context: The investigator wonders whether the robbers deleted a segment of the surveillance video.","presented_target":"<b>What does the investigator wonder whether the robbers deleted?<\/b>"},{"item":3,"block":1,"list":2,"condition":"SUBJ","Context":"The investigator thinks that a segment of the surveillance video was deleted by the robbers.","Target":"What does the investigator think that a segment of was deleted?","presented_context":"Context: The investigator thinks that a segment of the surveillance video was deleted by the robbers.","presented_target":"<b>What does the investigator think that a segment of was deleted?<\/b>"},{"item":4,"block":2,"list":1,"condition":"CNPC","Context":"The journalist believes the claim that the politician once wrote a book about astronomy.","Target":"What does the journalist believe the claim that the politician once wrote?","presented_context":"Context: The journalist believes the claim that the politician once wrote a book about astronomy.","presented_target":"<b>What does the journalist believe the claim that the politician once wrote?<\/b>"},{"item":4,"block":2,"list":2,"condition":"WH","Context":"The journalist wonders whether the politician once wrote a book about astronomy.","Target":"What does the journalist wonder whether the politician once wrote?","presented_context":"Context: The journalist wonders whether the politician once wrote a book about astronomy.","presented_target":"<b>What does the journalist wonder whether the politician once wrote?<\/b>"},{"item":4,"block":2,"list":3,"condition":"SUBJ","Context":"The journalist thinks that a book about astronomy was written by the politician. ","Target":"What does the journalist think that the book about was written by the politician?","presented_context":"Context: The journalist thinks that a book about astronomy was written by the politician. ","presented_target":"<b>What does the journalist think that the book about was written by the politician?<\/b>"},{"item":5,"block":2,"list":2,"condition":"CNPC","Context":"The police officer believes the claim that the truck hit a stack of hay.","Target":"What does the police officer believe the claim that the truck hit?","presented_context":"Context: The police officer believes the claim that the truck hit a stack of hay.","presented_target":"<b>What does the police officer believe the claim that the truck hit?<\/b>"},{"item":5,"block":2,"list":3,"condition":"WH","Context":"The police officer wonders whether the truck hit a stack of hay.","Target":"What does the police officer wonder whether the truck hit?","presented_context":"Context: The police officer wonders whether the truck hit a stack of hay.","presented_target":"<b>What does the police officer wonder whether the truck hit?<\/b>"},{"item":5,"block":2,"list":1,"condition":"SUBJ","Context":"The police officer thinks that a stack of hay was hit by the truck.","Target":"What does the police officer think that a stack of was hit by the truck?","presented_context":"Context: The police officer thinks that a stack of hay was hit by the truck.","presented_target":"<b>What does the police officer think that a stack of was hit by the truck?<\/b>"},{"item":6,"block":2,"list":3,"condition":"CNPC","Context":"The lawyer believes the claim that the scientist wrote an article about quantum physics. ","Target":"What does the lawyer believe the claim that the scientist wrote?","presented_context":"Context: The lawyer believes the claim that the scientist wrote an article about quantum physics. ","presented_target":"<b>What does the lawyer believe the claim that the scientist wrote?<\/b>"},{"item":6,"block":2,"list":1,"condition":"WH","Context":"The lawyer wonders whether the scientist wrote an article about quantum physics. ","Target":"What does the lawyer wonder whether the scientist wrote?","presented_context":"Context: The lawyer wonders whether the scientist wrote an article about quantum physics. ","presented_target":"<b>What does the lawyer wonder whether the scientist wrote?<\/b>"},{"item":6,"block":2,"list":2,"condition":"SUBJ","Context":"The lawyer thinks the article about quantum physics was written by a scientist. ","Target":"What does the lawyer think the article about was written by a scientist?","presented_context":"Context: The lawyer thinks the article about quantum physics was written by a scientist. ","presented_target":"<b>What does the lawyer think the article about was written by a scientist?<\/b>"},{"item":7,"block":3,"list":1,"condition":"CNPC","Context":"The manager believed the claim that the intern had taken a photo of the equipment.","Target":"What did the manager believe the claim that the intern had taken?","presented_context":"Context: The manager believed the claim that the intern had taken a photo of the equipment.","presented_target":"<b>What did the manager believe the claim that the intern had taken?<\/b>"},{"item":7,"block":3,"list":2,"condition":"WH","Context":"The manager wondered whether the intern had taken a photo of the equipment. ","Target":"What did the manager wonder whether the intern had taken?","presented_context":"Context: The manager wondered whether the intern had taken a photo of the equipment. ","presented_target":"<b>What did the manager wonder whether the intern had taken?<\/b>"},{"item":7,"block":3,"list":3,"condition":"SUBJ","Context":"The manager thinks that the photo of the equipment had been taken by an intern. ","Target":"What did the manager think that the photo of had been taken by an intern?","presented_context":"Context: The manager thinks that the photo of the equipment had been taken by an intern. ","presented_target":"<b>What did the manager think that the photo of had been taken by an intern?<\/b>"},{"item":8,"block":3,"list":2,"condition":"CNPC","Context":"The zookeeper believes the claim that a lion attacked a herd of antelopes yesterday. ","Target":"What does the zookeeper believe the claim that a lion attacked yesterday?","presented_context":"Context: The zookeeper believes the claim that a lion attacked a herd of antelopes yesterday. ","presented_target":"<b>What does the zookeeper believe the claim that a lion attacked yesterday?<\/b>"},{"item":8,"block":3,"list":3,"condition":"WH","Context":"The zookeeper wonders whether the lion attacked a herd of antelopes yesterday. ","Target":"What does the zookeper wonder whether the lion attacked?","presented_context":"Context: The zookeeper wonders whether the lion attacked a herd of antelopes yesterday. ","presented_target":"<b>What does the zookeper wonder whether the lion attacked?<\/b>"},{"item":8,"block":3,"list":1,"condition":"SUBJ","Context":"The zookeeper thinks that a herd of antelopes was attacked by a lion yesterday. ","Target":"What does the zookeeper think that a herd of was attacked by a lion yesterday?","presented_context":"Context: The zookeeper thinks that a herd of antelopes was attacked by a lion yesterday. ","presented_target":"<b>What does the zookeeper think that a herd of was attacked by a lion yesterday?<\/b>"},{"item":9,"block":3,"list":3,"condition":"CNPC","Context":"The publisher believed the claim that the famous author had already finished the draft of his new book.","Target":"What did the publisher believe the claim that the famous author had already finished?","presented_context":"Context: The publisher believed the claim that the famous author had already finished the draft of his new book.","presented_target":"<b>What did the publisher believe the claim that the famous author had already finished?<\/b>"},{"item":9,"block":3,"list":1,"condition":"WH","Context":"The publisher wondered whether the author had already finished the draft of his new book.","Target":"What did the publish wonder whether the author had already finished?","presented_context":"Context: The publisher wondered whether the author had already finished the draft of his new book.","presented_target":"<b>What did the publish wonder whether the author had already finished?<\/b>"},{"item":9,"block":3,"list":2,"condition":"SUBJ","Context":"The publisher thought that the draft of the author's new book was already finished. ","Target":"What did the publisher think that the draft of was already finished?","presented_context":"Context: The publisher thought that the draft of the author's new book was already finished. ","presented_target":"<b>What did the publisher think that the draft of was already finished?<\/b>"},{"item":10,"block":4,"list":1,"condition":"CNPC","Context":"The mayor believes the claim that the union is planning a protest against the new immigration law.","Target":"What does the mayor believe the claim that the union is planning a protest against?","presented_context":"Context: The mayor believes the claim that the union is planning a protest against the new immigration law.","presented_target":"<b>What does the mayor believe the claim that the union is planning a protest against?<\/b>"},{"item":10,"block":4,"list":2,"condition":"WH","Context":"The mayor wonders whether the union is planning a protest against the new immigration law. ","Target":"What does the mayor wonder whether the union is planning a protest against?","presented_context":"Context: The mayor wonders whether the union is planning a protest against the new immigration law. ","presented_target":"<b>What does the mayor wonder whether the union is planning a protest against?<\/b>"},{"item":10,"block":4,"list":3,"condition":"SUBJ","Context":"The mayor thinks that a protest against the new immigration law is being planned by the union. ","Target":"What does the mayor think a protest against is being planned by the union?","presented_context":"Context: The mayor thinks that a protest against the new immigration law is being planned by the union. ","presented_target":"<b>What does the mayor think a protest against is being planned by the union?<\/b>"},{"item":11,"block":4,"list":2,"condition":"CNPC","Context":"The mechanic believes the claim that the apprentice spilled the bottle of lubricant.","Target":"What does the mechanic believe the claim that the apprentice spilled?","presented_context":"Context: The mechanic believes the claim that the apprentice spilled the bottle of lubricant.","presented_target":"<b>What does the mechanic believe the claim that the apprentice spilled?<\/b>"},{"item":11,"block":4,"list":3,"condition":"WH","Context":"The mechanic wonders whether the apprentice spilled the bottle of lubricant.","Target":"What does the mechanic wonder whether the apprentice spilled?","presented_context":"Context: The mechanic wonders whether the apprentice spilled the bottle of lubricant.","presented_target":"<b>What does the mechanic wonder whether the apprentice spilled?<\/b>"},{"item":11,"block":4,"list":1,"condition":"SUBJ","Context":"The mechanic thinks that a bottle of lubricant was spilled by the apprentice. ","Target":"What does the mechanic think that a bottle of was spilled by the apprentice?","presented_context":"Context: The mechanic thinks that a bottle of lubricant was spilled by the apprentice. ","presented_target":"<b>What does the mechanic think that a bottle of was spilled by the apprentice?<\/b>"},{"item":12,"block":4,"list":3,"condition":"CNPC","Context":"The judge believed the claim that the police officer had emailed a report of the car crash to the prosecutor.","Target":"What did the judge believe the claim that the police officer had emailed to the prosecutor?","presented_context":"Context: The judge believed the claim that the police officer had emailed a report of the car crash to the prosecutor.","presented_target":"<b>What did the judge believe the claim that the police officer had emailed to the prosecutor?<\/b>"},{"item":12,"block":4,"list":1,"condition":"WH","Context":"The judge wondered whether the police officer had emailed a report of the car crash to the prosecutor.","Target":"What did the judge wonder whether the police officer had emailed to the prosecutor?","presented_context":"Context: The judge wondered whether the police officer had emailed a report of the car crash to the prosecutor.","presented_target":"<b>What did the judge wonder whether the police officer had emailed to the prosecutor?<\/b>"},{"item":12,"block":4,"list":2,"condition":"SUBJ","Context":"The judge thought that a report of the car crash had been emailed to the prosecutor.","Target":"What did the judge think that a report of had been emailed to the prosecutor?","presented_context":"Context: The judge thought that a report of the car crash had been emailed to the prosecutor.","presented_target":"<b>What did the judge think that a report of had been emailed to the prosecutor?<\/b>"},{"item":13,"block":5,"list":1,"condition":"CNPC","Context":"The athlete believes the claim that her manager called the agent of another athlete. ","Target":"Who does the athlete believe the claim that her manager called?","presented_context":"Context: The athlete believes the claim that her manager called the agent of another athlete. ","presented_target":"<b>Who does the athlete believe the claim that her manager called?<\/b>"},{"item":13,"block":5,"list":2,"condition":"WH","Context":"The athlete wonders whether her manager called the agent of another athlete. ","Target":"Who does the athlete wonder whether her manager called?","presented_context":"Context: The athlete wonders whether her manager called the agent of another athlete. ","presented_target":"<b>Who does the athlete wonder whether her manager called?<\/b>"},{"item":13,"block":5,"list":3,"condition":"SUBJ","Context":"The athlete thinks that the agent of another athlete called her manager. ","Target":"Who does the athlete think that the agent of called her manager?","presented_context":"Context: The athlete thinks that the agent of another athlete called her manager. ","presented_target":"<b>Who does the athlete think that the agent of called her manager?<\/b>"},{"item":14,"block":5,"list":2,"condition":"CNPC","Context":"The physician believes the claim that the group of researchers designed an effective vaccine for malaria. ","Target":"What does the physician believe the claim that the group of researchers designed?","presented_context":"Context: The physician believes the claim that the group of researchers designed an effective vaccine for malaria. ","presented_target":"<b>What does the physician believe the claim that the group of researchers designed?<\/b>"},{"item":14,"block":5,"list":3,"condition":"WH","Context":"The physician wonders whether the group of researchers designed an effective vaccine for malaria. ","Target":"What does the physician wonder whether the group of researchers designed?","presented_context":"Context: The physician wonders whether the group of researchers designed an effective vaccine for malaria. ","presented_target":"<b>What does the physician wonder whether the group of researchers designed?<\/b>"},{"item":14,"block":5,"list":1,"condition":"SUBJ","Context":"The physician thinks that an effective vaccine for malaria was designed by the group of researchers. ","Target":"What does the physician think that an effective vaccine for was designed by the group of researchers?","presented_context":"Context: The physician thinks that an effective vaccine for malaria was designed by the group of researchers. ","presented_target":"<b>What does the physician think that an effective vaccine for was designed by the group of researchers?<\/b>"},{"item":15,"block":5,"list":3,"condition":"CNPC","Context":"The prime minister believes the claim that a foreign spy stole the report of military expenditure.","Target":"What does the prime minister believe the claim that a foreign spy stole?","presented_context":"Context: The prime minister believes the claim that a foreign spy stole the report of military expenditure.","presented_target":"<b>What does the prime minister believe the claim that a foreign spy stole?<\/b>"},{"item":15,"block":5,"list":1,"condition":"WH","Context":"The prime minister wonders whether a foreign spy stole the report of military expenditure.","Target":"What does the prime minister wonder whether a foreign spy stole?","presented_context":"Context: The prime minister wonders whether a foreign spy stole the report of military expenditure.","presented_target":"<b>What does the prime minister wonder whether a foreign spy stole?<\/b>"},{"item":15,"block":5,"list":2,"condition":"SUBJ","Context":"The prime minister thinks that the report of military expenditure was stolen by a foreign spy. ","Target":"What does the prime minister think that the report of was stolen by a foreign spy?","presented_context":"Context: The prime minister thinks that the report of military expenditure was stolen by a foreign spy. ","presented_target":"<b>What does the prime minister think that the report of was stolen by a foreign spy?<\/b>"},{"item":16,"block":6,"list":1,"condition":"CNPC","Context":"The lexicographer believes the claim that some of his colleagues revised the dictionary for the endangered language. ","Target":"What does the lexicographer believe the claim that some of his colleagues revised?","presented_context":"Context: The lexicographer believes the claim that some of his colleagues revised the dictionary for the endangered language. ","presented_target":"<b>What does the lexicographer believe the claim that some of his colleagues revised?<\/b>"},{"item":16,"block":6,"list":2,"condition":"WH","Context":"The lexicographer wonders whether some of his colleagues revised the dictionary for the endangered language.","Target":"What does the lexicographer wonder whether some of his colleagues revised?","presented_context":"Context: The lexicographer wonders whether some of his colleagues revised the dictionary for the endangered language.","presented_target":"<b>What does the lexicographer wonder whether some of his colleagues revised?<\/b>"},{"item":16,"block":6,"list":3,"condition":"SUBJ","Context":"The lexicographer thinks that the dictionary for the endangered language was revised by some of his colleagues. ","Target":"What does the lexicographer think that the dictionary for was revised by some of his colleagues?","presented_context":"Context: The lexicographer thinks that the dictionary for the endangered language was revised by some of his colleagues. ","presented_target":"<b>What does the lexicographer think that the dictionary for was revised by some of his colleagues?<\/b>"},{"item":17,"block":6,"list":2,"condition":"CNPC","Context":"The doctor believes the claim that the mayor vetoed the proposal for a new hospital. ","Target":"What does the doctor believe the claim that the mayor vetoed?","presented_context":"Context: The doctor believes the claim that the mayor vetoed the proposal for a new hospital. ","presented_target":"<b>What does the doctor believe the claim that the mayor vetoed?<\/b>"},{"item":17,"block":6,"list":3,"condition":"WH","Context":"The doctor wonders whether the mayor vetoed the proposal for a new hospital.","Target":"What does the doctor wonder whether the mayor vetoed?","presented_context":"Context: The doctor wonders whether the mayor vetoed the proposal for a new hospital.","presented_target":"<b>What does the doctor wonder whether the mayor vetoed?<\/b>"},{"item":17,"block":6,"list":1,"condition":"SUBJ","Context":"The doctor thinks that the proposal for a new hospital was vetoed by the mayor. ","Target":"What does the doctor think that the proposal for was vetoed by the mayor?","presented_context":"Context: The doctor thinks that the proposal for a new hospital was vetoed by the mayor. ","presented_target":"<b>What does the doctor think that the proposal for was vetoed by the mayor?<\/b>"},{"item":18,"block":6,"list":3,"condition":"CNPC","Context":"The pilot believes the claim that the description of the hijacker matches the suspect.","Target":"Who does the pilot believe the claim that the description of the hijacker matches?","presented_context":"Context: The pilot believes the claim that the description of the hijacker matches the suspect.","presented_target":"<b>Who does the pilot believe the claim that the description of the hijacker matches?<\/b>"},{"item":18,"block":6,"list":1,"condition":"WH","Context":"The pilot wonders whether the description of the hijacker matches the suspect.","Target":"Who does the pilot wonder whether the description of the hijacker matches?","presented_context":"Context: The pilot wonders whether the description of the hijacker matches the suspect.","presented_target":"<b>Who does the pilot wonder whether the description of the hijacker matches?<\/b>"},{"item":18,"block":6,"list":2,"condition":"SUBJ","Context":"The pilot thinks that the description of the hijacker matches the suspect.","Target":"Who does the pilot think that the description of matches the suspect?","presented_context":"Context: The pilot thinks that the description of the hijacker matches the suspect.","presented_target":"<b>Who does the pilot think that the description of matches the suspect?<\/b>"},{"item":19,"block":7,"list":1,"condition":"CNPC","Context":"The football coach believes the claim that the brother of his best player has already contacted another coach.","Target":"Who does the football coach believe the claim that the brother of his best player has already contacted?","presented_context":"Context: The football coach believes the claim that the brother of his best player has already contacted another coach.","presented_target":"<b>Who does the football coach believe the claim that the brother of his best player has already contacted?<\/b>"},{"item":19,"block":7,"list":2,"condition":"WH","Context":"The football coach wonders whether the brother of his best player contacted another coach.","Target":"Who does the football coach wonder whether the brother of his best player contacted?","presented_context":"Context: The football coach wonders whether the brother of his best player contacted another coach.","presented_target":"<b>Who does the football coach wonder whether the brother of his best player contacted?<\/b>"},{"item":19,"block":7,"list":3,"condition":"SUBJ","Context":"The football coach thinks that the brother of his best player contacted another coach.","Target":"Who does the football coach think that the brother of contacted another coach?","presented_context":"Context: The football coach thinks that the brother of his best player contacted another coach.","presented_target":"<b>Who does the football coach think that the brother of contacted another coach?<\/b>"},{"item":20,"block":7,"list":2,"condition":"CNPC","Context":"The musician believes the claim that the record company will buy the copyrights to his songs.","Target":"What does the musician believe the claim that the record company will buy?","presented_context":"Context: The musician believes the claim that the record company will buy the copyrights to his songs.","presented_target":"<b>What does the musician believe the claim that the record company will buy?<\/b>"},{"item":20,"block":7,"list":3,"condition":"WH","Context":"The musician wonders whether the record company will buy the copyrights to his songs.","Target":"What does the musician wonder whether the record company will buy?","presented_context":"Context: The musician wonders whether the record company will buy the copyrights to his songs.","presented_target":"<b>What does the musician wonder whether the record company will buy?<\/b>"},{"item":20,"block":7,"list":1,"condition":"SUBJ","Context":"The musician thinks that the copyrights to his songs will be bought by the record company.","Target":"What does the musician think that the copyrights to will be bought by the record company?","presented_context":"Context: The musician thinks that the copyrights to his songs will be bought by the record company.","presented_target":"<b>What does the musician think that the copyrights to will be bought by the record company?<\/b>"},{"item":21,"block":7,"list":3,"condition":"CNPC","Context":"The chef believes the claim that the food critic will publish a positive review of his restaurant.","Target":"What does the chef believe the claim that the food critic will publish?","presented_context":"Context: The chef believes the claim that the food critic will publish a positive review of his restaurant.","presented_target":"<b>What does the chef believe the claim that the food critic will publish?<\/b>"},{"item":21,"block":7,"list":1,"condition":"WH","Context":"The chef wonders whether the food critic will publish a positive review of his restaurant.","Target":"What does the chef wonder whether the food critic will publish?","presented_context":"Context: The chef wonders whether the food critic will publish a positive review of his restaurant.","presented_target":"<b>What does the chef wonder whether the food critic will publish?<\/b>"},{"item":21,"block":7,"list":2,"condition":"SUBJ","Context":"The chef thinks that a positive review of his restaurant will be published by the food critic.","Target":"What does the chef think that a positive review of will be published by the food critic?","presented_context":"Context: The chef thinks that a positive review of his restaurant will be published by the food critic.","presented_target":"<b>What does the chef think that a positive review of will be published by the food critic?<\/b>"},{"item":22,"block":8,"list":1,"condition":"CNPC","Context":"The investors believe the claim that the construction company purchased a truckload of logs.","Target":"What do the investors believe the claim that the construction company purchased?","presented_context":"Context: The investors believe the claim that the construction company purchased a truckload of logs.","presented_target":"<b>What do the investors believe the claim that the construction company purchased?<\/b>"},{"item":22,"block":8,"list":2,"condition":"WH","Context":"The investors wonder whether the construction company purchased a truckload of logs.","Target":"What do the investors wonder whether the construction company purchased?","presented_context":"Context: The investors wonder whether the construction company purchased a truckload of logs.","presented_target":"<b>What do the investors wonder whether the construction company purchased?<\/b>"},{"item":22,"block":8,"list":3,"condition":"SUBJ","Context":"The investors think that a truckload of logs was purchased by the construction company.","Target":"What do the investors think that a truckload of was purchased by the construction company?","presented_context":"Context: The investors think that a truckload of logs was purchased by the construction company.","presented_target":"<b>What do the investors think that a truckload of was purchased by the construction company?<\/b>"},{"item":23,"block":8,"list":2,"condition":"CNPC","Context":"The biologist believes the claim that researchers will eventually find a cure to AIDS. ","Target":"What does the biologist believe the claim that researchers will eventually find?","presented_context":"Context: The biologist believes the claim that researchers will eventually find a cure to AIDS. ","presented_target":"<b>What does the biologist believe the claim that researchers will eventually find?<\/b>"},{"item":23,"block":8,"list":3,"condition":"WH","Context":"The biologist wonders whether researchers will eventually find a cure to AIDS.","Target":"What does the biologist wonder whether researchers will eventually find?","presented_context":"Context: The biologist wonders whether researchers will eventually find a cure to AIDS.","presented_target":"<b>What does the biologist wonder whether researchers will eventually find?<\/b>"},{"item":23,"block":8,"list":1,"condition":"SUBJ","Context":"The biologist thinks that a cure to AIDS will eventually be found by researchers. ","Target":"What does the biologist think that a cure to will eventually be found by researchers?","presented_context":"Context: The biologist thinks that a cure to AIDS will eventually be found by researchers. ","presented_target":"<b>What does the biologist think that a cure to will eventually be found by researchers?<\/b>"},{"item":24,"block":8,"list":3,"condition":"CNPC","Context":"The headmaster believes the claim that an expert in Chinese history wrote the manuscript.","Target":"What does the headmaster believe the claim that an expert in Chinese history wrote?","presented_context":"Context: The headmaster believes the claim that an expert in Chinese history wrote the manuscript.","presented_target":"<b>What does the headmaster believe the claim that an expert in Chinese history wrote?<\/b>"},{"item":24,"block":8,"list":1,"condition":"WH","Context":"The headmaster wonders whether an expert in Chinese history wrote the manuscript.","Target":"What does the headmaster wonder whether an expert in Chinese history wrote?","presented_context":"Context: The headmaster wonders whether an expert in Chinese history wrote the manuscript.","presented_target":"<b>What does the headmaster wonder whether an expert in Chinese history wrote?<\/b>"},{"item":24,"block":8,"list":2,"condition":"SUBJ","Context":"The headmaster thinks that an expert in Chinese history wrote the manuscript. ","Target":"What does the headmaster think that an expert in wrote the manuscript?","presented_context":"Context: The headmaster thinks that an expert in Chinese history wrote the manuscript. ","presented_target":"<b>What does the headmaster think that an expert in wrote the manuscript?<\/b>"},{"item":25,"block":9,"list":1,"condition":"CNPC","Context":"The villagers believed the claim that the news article on healthcare had caused the grave public unrest. ","Target":"What did the villagers believe the claim that the news article on healthcare had caused?","presented_context":"Context: The villagers believed the claim that the news article on healthcare had caused the grave public unrest. ","presented_target":"<b>What did the villagers believe the claim that the news article on healthcare had caused?<\/b>"},{"item":25,"block":9,"list":2,"condition":"WH","Context":"The villagers wondered whether the news article on healthcare had caused the grave public unrest. ","Target":"What did the villagers wonder whether the news article on healthcare had caused?","presented_context":"Context: The villagers wondered whether the news article on healthcare had caused the grave public unrest. ","presented_target":"<b>What did the villagers wonder whether the news article on healthcare had caused?<\/b>"},{"item":25,"block":9,"list":3,"condition":"SUBJ","Context":"The villagers thought that the news article on healthcare had caused the grave public unrest. ","Target":"What did the villagers think that the news article on had caused the grave public unrest?","presented_context":"Context: The villagers thought that the news article on healthcare had caused the grave public unrest. ","presented_target":"<b>What did the villagers think that the news article on had caused the grave public unrest?<\/b>"},{"item":26,"block":9,"list":2,"condition":"CNPC","Context":"The mechanic believes the claim that a tank of biofuel can power the locomotive.","Target":"What does the mechanic believe the claim that a tank of biofuel can power?","presented_context":"Context: The mechanic believes the claim that a tank of biofuel can power the locomotive.","presented_target":"<b>What does the mechanic believe the claim that a tank of biofuel can power?<\/b>"},{"item":26,"block":9,"list":3,"condition":"WH","Context":"The mechanic wonders whether a tank of biofuel can power the locomotive.","Target":"What does the mechanic wonder whether a tank of biofuel can power?","presented_context":"Context: The mechanic wonders whether a tank of biofuel can power the locomotive.","presented_target":"<b>What does the mechanic wonder whether a tank of biofuel can power?<\/b>"},{"item":26,"block":9,"list":1,"condition":"SUBJ","Context":"The mechanic thinks that a tank of biofuel can power the locomotive.","Target":"What does the mechanic think that a tank of can power the locomotive?","presented_context":"Context: The mechanic thinks that a tank of biofuel can power the locomotive.","presented_target":"<b>What does the mechanic think that a tank of can power the locomotive?<\/b>"},{"item":27,"block":9,"list":3,"condition":"CNPC","Context":"The pharmacist believed the claim that a pack of painkillers could cause nausea. ","Target":"What did the pharmacist believe the claim that a pack of painkillers could cause?","presented_context":"Context: The pharmacist believed the claim that a pack of painkillers could cause nausea. ","presented_target":"<b>What did the pharmacist believe the claim that a pack of painkillers could cause?<\/b>"},{"item":27,"block":9,"list":1,"condition":"WH","Context":"The pharmacist wondered whether a pack of painkillers could cause nausea.","Target":"What did the pharmacist wonder whether a pack of painkillers could cause?","presented_context":"Context: The pharmacist wondered whether a pack of painkillers could cause nausea.","presented_target":"<b>What did the pharmacist wonder whether a pack of painkillers could cause?<\/b>"},{"item":27,"block":9,"list":2,"condition":"SUBJ","Context":"The pharmacist thought that a pack of painkillers could cause nausea.","Target":"What did the pharmacist think that a pack of could cause nausea?","presented_context":"Context: The pharmacist thought that a pack of painkillers could cause nausea.","presented_target":"<b>What did the pharmacist think that a pack of could cause nausea?<\/b>"},{"item":28,"block":10,"list":1,"condition":"CNPC","Context":"The pianist believes the claim that two hours of piano practice per day can lead to perfection.","Target":"What does the pianist believe the claim that two hours of piano practice per day can lead to?","presented_context":"Context: The pianist believes the claim that two hours of piano practice per day can lead to perfection.","presented_target":"<b>What does the pianist believe the claim that two hours of piano practice per day can lead to?<\/b>"},{"item":28,"block":10,"list":2,"condition":"WH","Context":"The pianist wonders whether two hours of piano practice per day can lead to perfection.","Target":"What does the pianist wonder whether two hours of piano practice per day can lead to?","presented_context":"Context: The pianist wonders whether two hours of piano practice per day can lead to perfection.","presented_target":"<b>What does the pianist wonder whether two hours of piano practice per day can lead to?<\/b>"},{"item":28,"block":10,"list":3,"condition":"SUBJ","Context":"The pianist thinks that two hours of piano practice per day can lead to perfection.","Target":"What does the pianist think that two hours of per day can lead to perfection?","presented_context":"Context: The pianist thinks that two hours of piano practice per day can lead to perfection.","presented_target":"<b>What does the pianist think that two hours of per day can lead to perfection?<\/b>"},{"item":29,"block":10,"list":2,"condition":"CNPC","Context":"The detective believes the claim that a bottle of poison killed the businessman.","Target":"Who does the detective believe the claim that a bottle of poison killed?","presented_context":"Context: The detective believes the claim that a bottle of poison killed the businessman.","presented_target":"<b>Who does the detective believe the claim that a bottle of poison killed?<\/b>"},{"item":29,"block":10,"list":3,"condition":"WH","Context":"The detective wonders whether a bottle of poison killed the businessman.","Target":"What does the detective wonder whether a bottle of poison killed?","presented_context":"Context: The detective wonders whether a bottle of poison killed the businessman.","presented_target":"<b>What does the detective wonder whether a bottle of poison killed?<\/b>"},{"item":29,"block":10,"list":1,"condition":"SUBJ","Context":"The detective thinks that a bottle of poison killed the businessman.","Target":"What does the detective think that a bottle of killed the businessman?","presented_context":"Context: The detective thinks that a bottle of poison killed the businessman.","presented_target":"<b>What does the detective think that a bottle of killed the businessman?<\/b>"},{"item":30,"block":10,"list":3,"condition":"CNPC","Context":"The secretary believes the claim that the daughter of the congresswoman agreed with President.","Target":"Who does the secretary believe the claim that the daughter of the congresswoman agreed with?","presented_context":"Context: The secretary believes the claim that the daughter of the congresswoman agreed with President.","presented_target":"<b>Who does the secretary believe the claim that the daughter of the congresswoman agreed with?<\/b>"},{"item":30,"block":10,"list":1,"condition":"WH","Context":"The secretary wonders whether the daughter of the congresswoman agreed with the President.","Target":"Who does the secretary wonder whether the daughter of the congresswoman agreed with?","presented_context":"Context: The secretary wonders whether the daughter of the congresswoman agreed with the President.","presented_target":"<b>Who does the secretary wonder whether the daughter of the congresswoman agreed with?<\/b>"},{"item":30,"block":10,"list":2,"condition":"SUBJ","Context":"The secretary thinks that the daughter of the congresswoman agreed with the President.","Target":"Who does the secretary think that the daughter of agreed with the president?","presented_context":"Context: The secretary thinks that the daughter of the congresswoman agreed with the President.","presented_target":"<b>Who does the secretary think that the daughter of agreed with the president?<\/b>"},{"item":31,"block":11,"list":1,"condition":"CNPC","Context":"The radio host believes the claim that the ex-wife of the movie star bought a new house.","Target":"what does the radio host believe the claim that the ex-wife of the movie star bought?","presented_context":"Context: The radio host believes the claim that the ex-wife of the movie star bought a new house.","presented_target":"<b>what does the radio host believe the claim that the ex-wife of the movie star bought?<\/b>"},{"item":31,"block":11,"list":2,"condition":"WH","Context":"The radio host wonders whether the ex-wife of the movie star bought a new house.","Target":"What does the radio host wonder whether the ex-wife of the movie star bought?","presented_context":"Context: The radio host wonders whether the ex-wife of the movie star bought a new house.","presented_target":"<b>What does the radio host wonder whether the ex-wife of the movie star bought?<\/b>"},{"item":31,"block":11,"list":3,"condition":"SUBJ","Context":"The radio host thinks that the ex-wife of the move star bought a new house.","Target":"Who does the radio host think that the ex-wife of bought a new house?","presented_context":"Context: The radio host thinks that the ex-wife of the move star bought a new house.","presented_target":"<b>Who does the radio host think that the ex-wife of bought a new house?<\/b>"},{"item":32,"block":11,"list":2,"condition":"CNPC","Context":"The Prime Minister believes the claim that the senate will review the report on global warming.","Target":"What does the Prime Minister believe the claim that the senate will review?","presented_context":"Context: The Prime Minister believes the claim that the senate will review the report on global warming.","presented_target":"<b>What does the Prime Minister believe the claim that the senate will review?<\/b>"},{"item":32,"block":11,"list":3,"condition":"WH","Context":"The Prime Minister wonders whether the senate will review the report on global warming.","Target":"What does the Prime Minister wonder whether the senate will review?","presented_context":"Context: The Prime Minister wonders whether the senate will review the report on global warming.","presented_target":"<b>What does the Prime Minister wonder whether the senate will review?<\/b>"},{"item":32,"block":11,"list":1,"condition":"SUBJ","Context":"The Prime Minister thinks that the report on global warming will be reviewed by the senate.","Target":"What does the Prime Minister think that the report on will be reviewed by the senate?","presented_context":"Context: The Prime Minister thinks that the report on global warming will be reviewed by the senate.","presented_target":"<b>What does the Prime Minister think that the report on will be reviewed by the senate?<\/b>"},{"item":33,"block":11,"list":3,"condition":"CNPC","Context":"The nurse believes the claim that the treatment of a special skin condition mimics the effects of eczema.","Target":"What does the nurse believe the claim that the treatment of a special condition mimics?","presented_context":"Context: The nurse believes the claim that the treatment of a special skin condition mimics the effects of eczema.","presented_target":"<b>What does the nurse believe the claim that the treatment of a special condition mimics?<\/b>"},{"item":33,"block":11,"list":1,"condition":"WH","Context":"The nurse wonders whether the treatment of a special skin condition mimics the effects of eczema.","Target":"What does the nurse wonder whether the treatment of a special condition mimics?","presented_context":"Context: The nurse wonders whether the treatment of a special skin condition mimics the effects of eczema.","presented_target":"<b>What does the nurse wonder whether the treatment of a special condition mimics?<\/b>"},{"item":33,"block":11,"list":2,"condition":"SUBJ","Context":"The nuse thinks that the treatment of a special skin condition mimics the effects of eczema.","Target":"What does the nurse think that the treatment of mimics the effects of eczema?","presented_context":"Context: The nuse thinks that the treatment of a special skin condition mimics the effects of eczema.","presented_target":"<b>What does the nurse think that the treatment of mimics the effects of eczema?<\/b>"},{"item":34,"block":12,"list":1,"condition":"CNPC","Context":"The zoologist believes the claim that the mating call of the Gray Catbird resembles the sounds of a cat.","Target":"What does the zoologist believe the claim that the mating call of the Gray Catbird resembles?","presented_context":"Context: The zoologist believes the claim that the mating call of the Gray Catbird resembles the sounds of a cat.","presented_target":"<b>What does the zoologist believe the claim that the mating call of the Gray Catbird resembles?<\/b>"},{"item":34,"block":12,"list":2,"condition":"WH","Context":"The zoologist believes wonders whether the mating call of the Gray Catbird resembles the sounds of a cat.","Target":"What does the zoologist wonder whether the mating call of the Gray Catbird resembles?","presented_context":"Context: The zoologist believes wonders whether the mating call of the Gray Catbird resembles the sounds of a cat.","presented_target":"<b>What does the zoologist wonder whether the mating call of the Gray Catbird resembles?<\/b>"},{"item":34,"block":12,"list":3,"condition":"SUBJ","Context":"The zoologist thinks that the mating call of the mating call of the Gray Catebird resembles the sounds of a cat.","Target":"What does the zoologist think that the mating call of resembles the sounds of a cat?","presented_context":"Context: The zoologist thinks that the mating call of the mating call of the Gray Catebird resembles the sounds of a cat.","presented_target":"<b>What does the zoologist think that the mating call of resembles the sounds of a cat?<\/b>"},{"item":35,"block":12,"list":2,"condition":"CNPC","Context":"The boy believes the claim that the superhero will defeat the demons of the underworld.","Target":"Who does the boy believe the claim that the superhero will defeat?","presented_context":"Context: The boy believes the claim that the superhero will defeat the demons of the underworld.","presented_target":"<b>Who does the boy believe the claim that the superhero will defeat?<\/b>"},{"item":35,"block":12,"list":3,"condition":"WH","Context":"The boy wonders whether the superhero will defeat the demons of the underworld.","Target":"Who does the boy wonder whether the superhero will defeat?","presented_context":"Context: The boy wonders whether the superhero will defeat the demons of the underworld.","presented_target":"<b>Who does the boy wonder whether the superhero will defeat?<\/b>"},{"item":35,"block":12,"list":1,"condition":"SUBJ","Context":"The boy thinks that the demons of the underworld will be defeated by the superhero.","Target":"What does the boy think that the demons of will be defeated by the superhero?","presented_context":"Context: The boy thinks that the demons of the underworld will be defeated by the superhero.","presented_target":"<b>What does the boy think that the demons of will be defeated by the superhero?<\/b>"},{"item":36,"block":12,"list":3,"condition":"CNPC","Context":"The director believes the claim that the main actor will blow off the final rehearsal of the play.","Target":"What does the director believe the claim that the main actor will blow off?","presented_context":"Context: The director believes the claim that the main actor will blow off the final rehearsal of the play.","presented_target":"<b>What does the director believe the claim that the main actor will blow off?<\/b>"},{"item":36,"block":12,"list":1,"condition":"WH","Context":"The director wonders whether the main actor will blow off the final rehearsal of the play.","Target":"What does the director wonder whether the main actor will blow off?","presented_context":"Context: The director wonders whether the main actor will blow off the final rehearsal of the play.","presented_target":"<b>What does the director wonder whether the main actor will blow off?<\/b>"},{"item":36,"block":12,"list":2,"condition":"SUBJ","Context":"The director thinks that the final rehearsal of the play will be blown off by the main actor.","Target":"What does the director think that the final rehearsal of will be blown off by the main actor?","presented_context":"Context: The director thinks that the final rehearsal of the play will be blown off by the main actor.","presented_target":"<b>What does the director think that the final rehearsal of will be blown off by the main actor?<\/b>"},{"item":37,"block":13,"list":1,"condition":"CNPC","Context":"The delinquents believe the claim that the police arrested another group of delinquents.","Target":"Who do the delinquents believe the claim that the police arrested?","presented_context":"Context: The delinquents believe the claim that the police arrested another group of delinquents.","presented_target":"<b>Who do the delinquents believe the claim that the police arrested?<\/b>"},{"item":37,"block":13,"list":2,"condition":"WH","Context":"The delinquents wonder whether the police arrested another group of delinquents.","Target":"Who do the delinquents wonder whether the police arrested?","presented_context":"Context: The delinquents wonder whether the police arrested another group of delinquents.","presented_target":"<b>Who do the delinquents wonder whether the police arrested?<\/b>"},{"item":37,"block":13,"list":3,"condition":"SUBJ","Context":"The delinquents think that another group of delinquents was arrested by the police.","Target":"What do the delinquents think that another group of was arrested by the police?","presented_context":"Context: The delinquents think that another group of delinquents was arrested by the police.","presented_target":"<b>What do the delinquents think that another group of was arrested by the police?<\/b>"},{"item":38,"block":13,"list":2,"condition":"CNPC","Context":"The anesthesiologist believed the claim that the best surgeon of the hospital had committed malpractice.","Target":"What did the anesthesiologist believe the claim that the best surgeon of the hospital had committed?","presented_context":"Context: The anesthesiologist believed the claim that the best surgeon of the hospital had committed malpractice.","presented_target":"<b>What did the anesthesiologist believe the claim that the best surgeon of the hospital had committed?<\/b>"},{"item":38,"block":13,"list":3,"condition":"WH","Context":"The anesthesiologist wondered whether the best surgeon of the hospital had committed malpractice.","Target":"What did the anesthesiologist wonder whether the best surgeon of the hospital had committed?","presented_context":"Context: The anesthesiologist wondered whether the best surgeon of the hospital had committed malpractice.","presented_target":"<b>What did the anesthesiologist wonder whether the best surgeon of the hospital had committed?<\/b>"},{"item":38,"block":13,"list":1,"condition":"SUBJ","Context":"The anesthesiologist thought that the best surgeon of the hospital had committed malpractice.","Target":"What did the anesthesiologist think that the best surgeon of had committed malpractice?","presented_context":"Context: The anesthesiologist thought that the best surgeon of the hospital had committed malpractice.","presented_target":"<b>What did the anesthesiologist think that the best surgeon of had committed malpractice?<\/b>"},{"item":39,"block":13,"list":3,"condition":"CNPC","Context":" The activists believe the claim that government officials bribed the leader of the group.","Target":"Who do the activists believe the claim that government officials bribed?","presented_context":"Context:  The activists believe the claim that government officials bribed the leader of the group.","presented_target":"<b>Who do the activists believe the claim that government officials bribed?<\/b>"},{"item":39,"block":13,"list":1,"condition":"WH","Context":"The activists wonder whether government officials bribed the leader of the group.","Target":"Who do the activists wonder whether government officials bribed?","presented_context":"Context: The activists wonder whether government officials bribed the leader of the group.","presented_target":"<b>Who do the activists wonder whether government officials bribed?<\/b>"},{"item":39,"block":13,"list":2,"condition":"SUBJ","Context":"The activists think that the leader of the group was bribed by government officials.","Target":"What do the activists think that the leader of was bribed by government officials?","presented_context":"Context: The activists think that the leader of the group was bribed by government officials.","presented_target":"<b>What do the activists think that the leader of was bribed by government officials?<\/b>"},{"item":40,"block":14,"list":1,"condition":"CNPC","Context":"The janitor believes the claim that a bottle of bleach can remove the stain.","Target":"What does the janitor believe the claim that a bottle of bleach can remove?","presented_context":"Context: The janitor believes the claim that a bottle of bleach can remove the stain.","presented_target":"<b>What does the janitor believe the claim that a bottle of bleach can remove?<\/b>"},{"item":40,"block":14,"list":2,"condition":"WH","Context":"The janitor wonders whether a bottle of bleach can remove the stain.","Target":"What does the janitor wonder whether a bottle of bleach can remove?","presented_context":"Context: The janitor wonders whether a bottle of bleach can remove the stain.","presented_target":"<b>What does the janitor wonder whether a bottle of bleach can remove?<\/b>"},{"item":40,"block":14,"list":3,"condition":"SUBJ","Context":"The janitor thinks that a bottle of bleach can remove the stain.","Target":"What does the janitor think that a bottle of can remove the stain?","presented_context":"Context: The janitor thinks that a bottle of bleach can remove the stain.","presented_target":"<b>What does the janitor think that a bottle of can remove the stain?<\/b>"},{"item":41,"block":14,"list":2,"condition":"CNPC","Context":"The bartender believes the claim that the brother of the mayor invited a special guest.","Target":"Who does the bartender believe the claim that the brother of the mayor invited?","presented_context":"Context: The bartender believes the claim that the brother of the mayor invited a special guest.","presented_target":"<b>Who does the bartender believe the claim that the brother of the mayor invited?<\/b>"},{"item":41,"block":14,"list":3,"condition":"WH","Context":"The bartender wonders whether the brother of the mayor invited a special guest.","Target":"Who does the bartender wonder whether the brother of the mayor invited?","presented_context":"Context: The bartender wonders whether the brother of the mayor invited a special guest.","presented_target":"<b>Who does the bartender wonder whether the brother of the mayor invited?<\/b>"},{"item":41,"block":14,"list":1,"condition":"SUBJ","Context":"The bartender thinks that the brother of the mayor invited a special guest.","Target":"Who does the bartender think that the brother of invited a special guest?","presented_context":"Context: The bartender thinks that the brother of the mayor invited a special guest.","presented_target":"<b>Who does the bartender think that the brother of invited a special guest?<\/b>"},{"item":42,"block":14,"list":3,"condition":"CNPC","Context":"The spy believes the claim that the commander of the special forces initiated the attack.","Target":"What does the spy believe the claim that the commander of the special forces initiated?","presented_context":"Context: The spy believes the claim that the commander of the special forces initiated the attack.","presented_target":"<b>What does the spy believe the claim that the commander of the special forces initiated?<\/b>"},{"item":42,"block":14,"list":1,"condition":"WH","Context":"The spy wonders whether the commander of the special forces initiated the attack.","Target":"What does the spy wonder whether the commander of the special forces initiated?","presented_context":"Context: The spy wonders whether the commander of the special forces initiated the attack.","presented_target":"<b>What does the spy wonder whether the commander of the special forces initiated?<\/b>"},{"item":42,"block":14,"list":2,"condition":"SUBJ","Context":"The spy thinks that the commander of the special forces initiated the attack.","Target":"What does the spy think that the commander of initiated the attack?","presented_context":"Context: The spy thinks that the commander of the special forces initiated the attack.","presented_target":"<b>What does the spy think that the commander of initiated the attack?<\/b>"},{"item":43,"block":15,"list":1,"condition":"CNPC","Context":"The carpenter believes the claim that the sister of the mayor will order a bookcase.","Target":"What does the carpenter believe the claim that the sister of the mayor will order?","presented_context":"Context: The carpenter believes the claim that the sister of the mayor will order a bookcase.","presented_target":"<b>What does the carpenter believe the claim that the sister of the mayor will order?<\/b>"},{"item":43,"block":15,"list":2,"condition":"WH","Context":"The carpenter wonders whether the sister of the mayor will order a bookcase.","Target":"What does the carpenter wonder whether the sister of the mayor will order?","presented_context":"Context: The carpenter wonders whether the sister of the mayor will order a bookcase.","presented_target":"<b>What does the carpenter wonder whether the sister of the mayor will order?<\/b>"},{"item":43,"block":15,"list":3,"condition":"SUBJ","Context":"The carpenter thinks that the sister of the mayor will order a bookcase.","Target":"Who does the carpenter think that the sister of will order a bookcase?","presented_context":"Context: The carpenter thinks that the sister of the mayor will order a bookcase.","presented_target":"<b>Who does the carpenter think that the sister of will order a bookcase?<\/b>"},{"item":44,"block":15,"list":2,"condition":"CNPC","Context":"The presidential candidate believes the claim that the report of the accident scared his opponent.","Target":"Who does the presidential candidate believe the claim that the report of the accident scared?","presented_context":"Context: The presidential candidate believes the claim that the report of the accident scared his opponent.","presented_target":"<b>Who does the presidential candidate believe the claim that the report of the accident scared?<\/b>"},{"item":44,"block":15,"list":3,"condition":"WH","Context":"The presidential candidate wonders whether the report of the accident scared his opponent.","Target":"Who does the presidential candidate wonder whether the report of the accident scared?","presented_context":"Context: The presidential candidate wonders whether the report of the accident scared his opponent.","presented_target":"<b>Who does the presidential candidate wonder whether the report of the accident scared?<\/b>"},{"item":44,"block":15,"list":1,"condition":"SUBJ","Context":"The presidential candidate thinks that the report of the accident scared his opponent.","Target":"What does the presidential candidate think that the report of scared his opponent?","presented_context":"Context: The presidential candidate thinks that the report of the accident scared his opponent.","presented_target":"<b>What does the presidential candidate think that the report of scared his opponent?<\/b>"},{"item":45,"block":15,"list":3,"condition":"CNPC","Context":"The actor believes the claim that the famous scholar wrote a book about cinematography. ","Target":"What does the actor believe the claim that the famous scholar wrote?","presented_context":"Context: The actor believes the claim that the famous scholar wrote a book about cinematography. ","presented_target":"<b>What does the actor believe the claim that the famous scholar wrote?<\/b>"},{"item":45,"block":15,"list":1,"condition":"WH","Context":"The actor wonders whether the famous scholar wrote a book about cinematography.","Target":"What does the actor wonder whether the famouse scholar wrote?","presented_context":"Context: The actor wonders whether the famous scholar wrote a book about cinematography.","presented_target":"<b>What does the actor wonder whether the famouse scholar wrote?<\/b>"},{"item":45,"block":15,"list":2,"condition":"SUBJ","Context":"The actor thinks that the book about cinematography was written by the famous scholar.","Target":"What does the actor think that a book about was written by the famous scholar?","presented_context":"Context: The actor thinks that the book about cinematography was written by the famous scholar.","presented_target":"<b>What does the actor think that a book about was written by the famous scholar?<\/b>"},{"item":101,"block":1,"list":1,"condition":"FILL","Context":"The police officer believes the claim that the bystanders witnessed the traffic accident.","Target":"Who believes the claim that the bystanders witnessed the traffic accident?","presented_context":"Context: The police officer believes the claim that the bystanders witnessed the traffic accident.","presented_target":"<b>Who believes the claim that the bystanders witnessed the traffic accident?<\/b>"},{"item":102,"block":2,"list":1,"condition":"FILL","Context":"The customer believed the claim that the waiter mixed up all the orders.","Target":"Who believed the claim that the waiter mixed up all the orders?","presented_context":"Context: The customer believed the claim that the waiter mixed up all the orders.","presented_target":"<b>Who believed the claim that the waiter mixed up all the orders?<\/b>"},{"item":103,"block":3,"list":1,"condition":"FILL","Context":"The celebrity believes the claim that the reporter hid in the bushes to photograph her new house.","Target":"Who believes the claim that the reporter hid in the bushes to photograph her new house?","presented_context":"Context: The celebrity believes the claim that the reporter hid in the bushes to photograph her new house.","presented_target":"<b>Who believes the claim that the reporter hid in the bushes to photograph her new house?<\/b>"},{"item":104,"block":4,"list":1,"condition":"FILL","Context":"The mayor believed the claim that the fireman had to smash a window to gain entrance to the house on fire.","Target":"Who believed the claim that the fireman had to smash a window to gain entrance to the house on fire?","presented_context":"Context: The mayor believed the claim that the fireman had to smash a window to gain entrance to the house on fire.","presented_target":"<b>Who believed the claim that the fireman had to smash a window to gain entrance to the house on fire?<\/b>"},{"item":105,"block":5,"list":1,"condition":"FILL","Context":"The suspect wonders whether the witness told the detectives what happened last night.","Target":"Who wonders whether the witness told the detectives what happened last night?","presented_context":"Context: The suspect wonders whether the witness told the detectives what happened last night.","presented_target":"<b>Who wonders whether the witness told the detectives what happened last night?<\/b>"},{"item":106,"block":6,"list":1,"condition":"FILL","Context":"The student wondered whether the teacher would bring a box of chocalate to class the next day.","Target":"Who wondered whether the teacher would bring a box of chocalate to class the next day?","presented_context":"Context: The student wondered whether the teacher would bring a box of chocalate to class the next day.","presented_target":"<b>Who wondered whether the teacher would bring a box of chocalate to class the next day?<\/b>"},{"item":107,"block":7,"list":1,"condition":"FILL","Context":"The inspector wonders whether the engineer pressed a button to activate the new machine.","Target":"Who wonders whether the engineer pressed a button to activate the new machine?","presented_context":"Context: The inspector wonders whether the engineer pressed a button to activate the new machine.","presented_target":"<b>Who wonders whether the engineer pressed a button to activate the new machine?<\/b>"},{"item":108,"block":8,"list":1,"condition":"FILL","Context":"The guests wondered whether vegetarian dishes will be served at the dinner party.","Target":"Who wondered whether vegetarian dishes will be served at the dinner party?","presented_context":"Context: The guests wondered whether vegetarian dishes will be served at the dinner party.","presented_target":"<b>Who wondered whether vegetarian dishes will be served at the dinner party?<\/b>"},{"item":109,"block":9,"list":1,"condition":"FILL","Context":"The patient thinks that the doctor decided not to treat the mysterious condition.","Target":"Who thinks that the doctor decided not to treat the mysterious condition?","presented_context":"Context: The patient thinks that the doctor decided not to treat the mysterious condition.","presented_target":"<b>Who thinks that the doctor decided not to treat the mysterious condition?<\/b>"},{"item":110,"block":10,"list":1,"condition":"FILL","Context":"The editor thinks that the journalist should be held responsible for the typos in the article.","Target":"Who thinks that the journalist should be held responsible for the typos in the article?","presented_context":"Context: The editor thinks that the journalist should be held responsible for the typos in the article.","presented_target":"<b>Who thinks that the journalist should be held responsible for the typos in the article?<\/b>"},{"item":111,"block":11,"list":1,"condition":"FILL","Context":"The soldier thought the general should order his troops to retreat.","Target":"Who did the soldier think should order his troops to retreat?","presented_context":"Context: The soldier thought the general should order his troops to retreat.","presented_target":"<b>Who did the soldier think should order his troops to retreat?<\/b>"},{"item":112,"block":12,"list":1,"condition":"FILL","Context":"The soccer player thinks the manager will be fired by the club chairman.","Target":"Who does the soccer player thinks will be fired by the club chairman?","presented_context":"Context: The soccer player thinks the manager will be fired by the club chairman.","presented_target":"<b>Who does the soccer player thinks will be fired by the club chairman?<\/b>"},{"item":113,"block":13,"list":1,"condition":"FILL","Context":"The chef believes that the restaurant owner is planning to open a new branch.","Target":"What does the chef believe that the restaurant owner is planning to open?","presented_context":"Context: The chef believes that the restaurant owner is planning to open a new branch.","presented_target":"<b>What does the chef believe that the restaurant owner is planning to open?<\/b>"},{"item":114,"block":14,"list":1,"condition":"FILL","Context":"The banker believed that the customer took out a loan to purchase a new car.","Target":"What did the banker believe that the customer took out a loan to purchase?","presented_context":"Context: The banker believed that the customer took out a loan to purchase a new car.","presented_target":"<b>What did the banker believe that the customer took out a loan to purchase?<\/b>"},{"item":115,"block":15,"list":1,"condition":"FILL","Context":"The composer thinks that the violinist has three extra tickets to the concert.","Target":"What does the composer think that the violinist has three extra tickets to?","presented_context":"Context: The composer thinks that the violinist has three extra tickets to the concert.","presented_target":"<b>What does the composer think that the violinist has three extra tickets to?<\/b>"},{"item":131,"block":1,"list":2,"condition":"FILL","Context":"The economist wondered whether raising personal income tax would help the economy.","Target":"Who wondered whether raising personal income tax would help the economy?","presented_context":"Context: The economist wondered whether raising personal income tax would help the economy.","presented_target":"<b>Who wondered whether raising personal income tax would help the economy?<\/b>"},{"item":132,"block":2,"list":2,"condition":"FILL","Context":"The cashier thinks that the new promotions will attract more customers.","Target":"Who thinks that the new promotions will attract more customers?","presented_context":"Context: The cashier thinks that the new promotions will attract more customers.","presented_target":"<b>Who thinks that the new promotions will attract more customers?<\/b>"},{"item":133,"block":3,"list":2,"condition":"FILL","Context":"The air traffic controller believes the claim that most flights will be cancelled due to the blizzard.","Target":"Who believes the claim that most flights will be cancelled due to the blizzard?","presented_context":"Context: The air traffic controller believes the claim that most flights will be cancelled due to the blizzard.","presented_target":"<b>Who believes the claim that most flights will be cancelled due to the blizzard?<\/b>"},{"item":134,"block":4,"list":2,"condition":"FILL","Context":"The nutritionist wonders whether his client benefited from the dietary supplements.","Target":"Who wonders whether his client benefited from the dietary supplements?","presented_context":"Context: The nutritionist wonders whether his client benefited from the dietary supplements.","presented_target":"<b>Who wonders whether his client benefited from the dietary supplements?<\/b>"},{"item":135,"block":5,"list":2,"condition":"FILL","Context":"The receptionist thought that the office should remain open during Christmas.","Target":"Who thought that the office should remain open during Christmas?","presented_context":"Context: The receptionist thought that the office should remain open during Christmas.","presented_target":"<b>Who thought that the office should remain open during Christmas?<\/b>"},{"item":136,"block":6,"list":2,"condition":"FILL","Context":"The construction worker believes the claim that companies should allow their employees to unionize.","Target":"Who believes the claim that companies should allow their employees to unionize?","presented_context":"Context: The construction worker believes the claim that companies should allow their employees to unionize.","presented_target":"<b>Who believes the claim that companies should allow their employees to unionize?<\/b>"},{"item":137,"block":7,"list":2,"condition":"FILL","Context":"The ecologist wonders whether the Amazon rainforest is shrinking every year.","Target":"Who wonders whether the Amazon rainforest is shrinking every year?","presented_context":"Context: The ecologist wonders whether the Amazon rainforest is shrinking every year.","presented_target":"<b>Who wonders whether the Amazon rainforest is shrinking every year?<\/b>"},{"item":138,"block":8,"list":2,"condition":"FILL","Context":"The neuroscientist thinks that the politician is probably suffering from dementia. ","Target":"Who thinks that the politician is probably suffering from dementia?","presented_context":"Context: The neuroscientist thinks that the politician is probably suffering from dementia. ","presented_target":"<b>Who thinks that the politician is probably suffering from dementia?<\/b>"},{"item":139,"block":9,"list":2,"condition":"FILL","Context":"The astronaut believes that the spaceship requires monthly maintenance in order to function properly.","Target":"Who believes that the spaceship requires monthly maintenance in order to function properly?","presented_context":"Context: The astronaut believes that the spaceship requires monthly maintenance in order to function properly.","presented_target":"<b>Who believes that the spaceship requires monthly maintenance in order to function properly?<\/b>"},{"item":140,"block":10,"list":2,"condition":"FILL","Context":"The vice president wonders whether the president will sign the peace treaty with the rebels. ","Target":"Who wonders whether the president will sign the peace trety with the rebels?","presented_context":"Context: The vice president wonders whether the president will sign the peace treaty with the rebels. ","presented_target":"<b>Who wonders whether the president will sign the peace trety with the rebels?<\/b>"},{"item":141,"block":11,"list":2,"condition":"FILL","Context":"The autoworker thinks that the factory should remain open during the economic recession. ","Target":"Who thinks that the factory should remain open during the economic recession?","presented_context":"Context: The autoworker thinks that the factory should remain open during the economic recession. ","presented_target":"<b>Who thinks that the factory should remain open during the economic recession?<\/b>"},{"item":142,"block":12,"list":2,"condition":"FILL","Context":"The polyglot believes that Dutch would be an easy language for English speakers to learn. ","Target":"Who believes that Dutch would be an easy language for English speakers to learn?","presented_context":"Context: The polyglot believes that Dutch would be an easy language for English speakers to learn. ","presented_target":"<b>Who believes that Dutch would be an easy language for English speakers to learn?<\/b>"},{"item":143,"block":13,"list":2,"condition":"FILL","Context":"The air marshal wonders whether the hijackers will release the hostages. ","Target":"Who wonders whether the hijackers will release the hostages?","presented_context":"Context: The air marshal wonders whether the hijackers will release the hostages. ","presented_target":"<b>Who wonders whether the hijackers will release the hostages?<\/b>"},{"item":144,"block":14,"list":2,"condition":"FILL","Context":"The old lady thinks that there are not enough parking spots near the shopping mall.","Target":"Who thinks that there are not enough parking spots near the shopping mall?","presented_context":"Context: The old lady thinks that there are not enough parking spots near the shopping mall.","presented_target":"<b>Who thinks that there are not enough parking spots near the shopping mall?<\/b>"},{"item":145,"block":15,"list":2,"condition":"FILL","Context":"The dog trainer believes that the animal shelter should remain open during Thanksgiving. ","Target":"Who believes that the animal shelter should remain open during Thanksgiving?","presented_context":"Context: The dog trainer believes that the animal shelter should remain open during Thanksgiving. ","presented_target":"<b>Who believes that the animal shelter should remain open during Thanksgiving?<\/b>"},{"item":101,"block":1,"list":3,"condition":"FILL","Context":"The police officer believes the claim that the bystanders witnessed the traffic accident.","Target":"Who believes the claim that the bystanders witnessed the traffic accident?","presented_context":"Context: The police officer believes the claim that the bystanders witnessed the traffic accident.","presented_target":"<b>Who believes the claim that the bystanders witnessed the traffic accident?<\/b>"},{"item":102,"block":2,"list":3,"condition":"FILL","Context":"The customer believed the claim that the waiter mixed up all the orders.","Target":"Who believed the claim that the waiter mixed up all the orders?","presented_context":"Context: The customer believed the claim that the waiter mixed up all the orders.","presented_target":"<b>Who believed the claim that the waiter mixed up all the orders?<\/b>"},{"item":103,"block":3,"list":3,"condition":"FILL","Context":"The celebrity believes the claim that the reporter hid in the bushes to photograph her new house.","Target":"Who believes the claim that the reporter hid in the bushes to photograph her new house?","presented_context":"Context: The celebrity believes the claim that the reporter hid in the bushes to photograph her new house.","presented_target":"<b>Who believes the claim that the reporter hid in the bushes to photograph her new house?<\/b>"},{"item":104,"block":4,"list":3,"condition":"FILL","Context":"The mayor believed the claim that the fireman had to smash a window to gain entrance to the house on fire.","Target":"Who believed the claim that the fireman had to smash a window to gain entrance to the house on fire?","presented_context":"Context: The mayor believed the claim that the fireman had to smash a window to gain entrance to the house on fire.","presented_target":"<b>Who believed the claim that the fireman had to smash a window to gain entrance to the house on fire?<\/b>"},{"item":105,"block":5,"list":3,"condition":"FILL","Context":"The suspect wonders whether the witness told the detectives what happened last night.","Target":"Who wonders whether the witness told the detectives what happened last night?","presented_context":"Context: The suspect wonders whether the witness told the detectives what happened last night.","presented_target":"<b>Who wonders whether the witness told the detectives what happened last night?<\/b>"},{"item":106,"block":6,"list":3,"condition":"FILL","Context":"The student wondered whether the teacher would bring a box of chocalate to class the next day.","Target":"Who wondered whether the teacher would bring a box of chocalate to class the next day?","presented_context":"Context: The student wondered whether the teacher would bring a box of chocalate to class the next day.","presented_target":"<b>Who wondered whether the teacher would bring a box of chocalate to class the next day?<\/b>"},{"item":107,"block":7,"list":3,"condition":"FILL","Context":"The inspector wonders whether the engineer pressed a button to activate the new machine.","Target":"Who wonders whether the engineer pressed a button to activate the new machine?","presented_context":"Context: The inspector wonders whether the engineer pressed a button to activate the new machine.","presented_target":"<b>Who wonders whether the engineer pressed a button to activate the new machine?<\/b>"},{"item":108,"block":8,"list":3,"condition":"FILL","Context":"The guests wondered whether vegetarian dishes will be served at the dinner party.","Target":"Who wondered whether vegetarian dishes will be served at the dinner party?","presented_context":"Context: The guests wondered whether vegetarian dishes will be served at the dinner party.","presented_target":"<b>Who wondered whether vegetarian dishes will be served at the dinner party?<\/b>"},{"item":109,"block":9,"list":3,"condition":"FILL","Context":"The patient thinks that the doctor decided not to treat the mysterious condition.","Target":"Who thinks that the doctor decided not to treat the mysterious condition?","presented_context":"Context: The patient thinks that the doctor decided not to treat the mysterious condition.","presented_target":"<b>Who thinks that the doctor decided not to treat the mysterious condition?<\/b>"},{"item":110,"block":10,"list":3,"condition":"FILL","Context":"The editor thinks that the journalist should be held responsible for the typos in the article.","Target":"Who thinks that the journalist should be held responsible for the typos in the article?","presented_context":"Context: The editor thinks that the journalist should be held responsible for the typos in the article.","presented_target":"<b>Who thinks that the journalist should be held responsible for the typos in the article?<\/b>"},{"item":111,"block":11,"list":3,"condition":"FILL","Context":"The soldier thought the general should order his troops to retreat.","Target":"Who did the soldier think should order his troops to retreat?","presented_context":"Context: The soldier thought the general should order his troops to retreat.","presented_target":"<b>Who did the soldier think should order his troops to retreat?<\/b>"},{"item":112,"block":12,"list":3,"condition":"FILL","Context":"The soccer player thinks the manager will be fired by the club chairman.","Target":"Who does the soccer player thinks will be fired by the club chairman?","presented_context":"Context: The soccer player thinks the manager will be fired by the club chairman.","presented_target":"<b>Who does the soccer player thinks will be fired by the club chairman?<\/b>"},{"item":113,"block":13,"list":3,"condition":"FILL","Context":"The chef believes that the restaurant owner is planning to open a new branch.","Target":"What does the chef believe that the restaurant owner is planning to open?","presented_context":"Context: The chef believes that the restaurant owner is planning to open a new branch.","presented_target":"<b>What does the chef believe that the restaurant owner is planning to open?<\/b>"},{"item":114,"block":14,"list":3,"condition":"FILL","Context":"The banker believed that the customer took out a loan to purchase a new car.","Target":"What did the banker believe that the customer took out a loan to purchase?","presented_context":"Context: The banker believed that the customer took out a loan to purchase a new car.","presented_target":"<b>What did the banker believe that the customer took out a loan to purchase?<\/b>"},{"item":115,"block":15,"list":3,"condition":"FILL","Context":"The composer thinks that the violinist has three extra tickets to the concert.","Target":"What does the composer think that the violinist has three extra tickets to?","presented_context":"Context: The composer thinks that the violinist has three extra tickets to the concert.","presented_target":"<b>What does the composer think that the violinist has three extra tickets to?<\/b>"},{"item":116,"block":1,"list":1,"condition":"UNGRAM","Context":"The city officials are trying to find out the source of the virus outbreak.","Target":"What out find to trying officials are the city?","presented_context":"Context: The city officials are trying to find out the source of the virus outbreak.","presented_target":"<b>What out find to trying officials are the city?<\/b>"},{"item":117,"block":2,"list":1,"condition":"UNGRAM","Context":"The software engineer thinks that there is a bug in the system.","Target":"Who there is a the bug  system in  thinks that?","presented_context":"Context: The software engineer thinks that there is a bug in the system.","presented_target":"<b>Who there is a the bug  system in  thinks that?<\/b>"},{"item":118,"block":3,"list":1,"condition":"UNGRAM","Context":"The presidential candidate acknowledged the possibility that there might be an error in the poll result. ","Target":"Who result poll the in error an be might there that acknowledged?","presented_context":"Context: The presidential candidate acknowledged the possibility that there might be an error in the poll result. ","presented_target":"<b>Who result poll the in error an be might there that acknowledged?<\/b>"},{"item":119,"block":4,"list":1,"condition":"UNGRAM","Context":"The trainers believe the claim that the team needs a more sophisticated training regimen.","Target":"What team do needs trainers the that believe the?","presented_context":"Context: The trainers believe the claim that the team needs a more sophisticated training regimen.","presented_target":"<b>What team do needs trainers the that believe the?<\/b>"},{"item":120,"block":5,"list":1,"condition":"UNGRAM","Context":"The analyst wonders whether the level of competition on college sports has become fiercer.","Target":"Who fiercer whether competition sports become the college level in has wonders?","presented_context":"Context: The analyst wonders whether the level of competition on college sports has become fiercer.","presented_target":"<b>Who fiercer whether competition sports become the college level in has wonders?<\/b>"},{"item":121,"block":6,"list":1,"condition":"UNGRAM","Context":"The children who were malnourished recovered from the flu.","Target":"What from malnourished were recover did the children who?","presented_context":"Context: The children who were malnourished recovered from the flu.","presented_target":"<b>What from malnourished were recover did the children who?<\/b>"},{"item":122,"block":7,"list":1,"condition":"UNGRAM","Context":"The police officer wonders whether the bystanders witnessed the terrifying incident.","Target":"Who whether incident witnessed wonders the bystanders the terrible?","presented_context":"Context: The police officer wonders whether the bystanders witnessed the terrifying incident.","presented_target":"<b>Who whether incident witnessed wonders the bystanders the terrible?<\/b>"},{"item":123,"block":8,"list":1,"condition":"UNGRAM","Context":"The waiter recommended the new cocktail to the guests who sat on the patio.","Target":"What patio the on sat who guests waiter did the recommend to?","presented_context":"Context: The waiter recommended the new cocktail to the guests who sat on the patio.","presented_target":"<b>What patio the on sat who guests waiter did the recommend to?<\/b>"},{"item":124,"block":9,"list":1,"condition":"UNGRAM","Context":"The reporter thinks that the politician will give a speech at the fundraising event.","Target":"Who event fundraising at thinks the that politician the a speech will give?","presented_context":"Context: The reporter thinks that the politician will give a speech at the fundraising event.","presented_target":"<b>Who event fundraising at thinks the that politician the a speech will give?<\/b>"},{"item":125,"block":10,"list":1,"condition":"UNGRAM","Context":"The tourist wondered whether the art exhibition at the city musuem was worth going to.","Target":"Who worth to going is whether  the art  exhibition wondered city the musuem at?","presented_context":"Context: The tourist wondered whether the art exhibition at the city musuem was worth going to.","presented_target":"<b>Who worth to going is whether  the art  exhibition wondered city the musuem at?<\/b>"},{"item":126,"block":11,"list":1,"condition":"UNGRAM","Context":"The cook believes the claim that the restaurant did not pass the health inspection.","Target":"Who the inspection the did not  restaurant pass health believes the claim that?","presented_context":"Context: The cook believes the claim that the restaurant did not pass the health inspection.","presented_target":"<b>Who the inspection the did not  restaurant pass health believes the claim that?<\/b>"},{"item":127,"block":12,"list":1,"condition":"UNGRAM","Context":"The dentist thinks that the senator should vote for the proposed health care reform.","Target":"Who senator proposed should thinks the that vote health care reform for the?","presented_context":"Context: The dentist thinks that the senator should vote for the proposed health care reform.","presented_target":"<b>Who senator proposed should thinks the that vote health care reform for the?<\/b>"},{"item":128,"block":13,"list":1,"condition":"UNGRAM","Context":"The guitarist wonders whether the drummer will come to the rehearsal.","Target":"Who to the wonders rehearsal the whether drummer come will?","presented_context":"Context: The guitarist wonders whether the drummer will come to the rehearsal.","presented_target":"<b>Who to the wonders rehearsal the whether drummer come will?<\/b>"},{"item":129,"block":14,"list":1,"condition":"UNGRAM","Context":"The sheriff believes the claim that the kidnapper is hiding in the mall.","Target":"Who that the in mall the kidnapper is hiding believes the claim?","presented_context":"Context: The sheriff believes the claim that the kidnapper is hiding in the mall.","presented_target":"<b>Who that the in mall the kidnapper is hiding believes the claim?<\/b>"},{"item":130,"block":15,"list":1,"condition":"UNGRAM","Context":"The nuclear physicist thinks that the new nuclear power plant is extremely safe.","Target":"Who safe thinks plant new that the is extremely power nuclear?","presented_context":"Context: The nuclear physicist thinks that the new nuclear power plant is extremely safe.","presented_target":"<b>Who safe thinks plant new that the is extremely power nuclear?<\/b>"},{"item":140,"block":1,"list":2,"condition":"UNGRAM","Context":"The vice president wonders whether the president will sign the peace treaty with the rebels. ","Target":"Who whether the president the peace trety with wonders will sign the rebels?","presented_context":"Context: The vice president wonders whether the president will sign the peace treaty with the rebels. ","presented_target":"<b>Who whether the president the peace trety with wonders will sign the rebels?<\/b>"},{"item":141,"block":2,"list":2,"condition":"UNGRAM","Context":"The autoworker thinks that the factory should remain open during the economic recession. ","Target":"Who during the factory should  thinks that remain open the economic recession?","presented_context":"Context: The autoworker thinks that the factory should remain open during the economic recession. ","presented_target":"<b>Who during the factory should  thinks that remain open the economic recession?<\/b>"},{"item":142,"block":3,"list":2,"condition":"UNGRAM","Context":"The polyglot believes that Dutch would be an easy language for English speakers to learn. ","Target":"Who be would Dutch language easy an believes that for English to speakers learn?","presented_context":"Context: The polyglot believes that Dutch would be an easy language for English speakers to learn. ","presented_target":"<b>Who be would Dutch language easy an believes that for English to speakers learn?<\/b>"},{"item":143,"block":4,"list":2,"condition":"UNGRAM","Context":"The air marshal wonders whether the hijackers will release the hostages. ","Target":"Who hijackers wonders release whether hostages the will the?","presented_context":"Context: The air marshal wonders whether the hijackers will release the hostages. ","presented_target":"<b>Who hijackers wonders release whether hostages the will the?<\/b>"},{"item":144,"block":5,"list":2,"condition":"UNGRAM","Context":"The old lady thinks that there are not enough parking spots near the shopping mall.","Target":"Who spots near the shopping parking thinks that there are not enough mall?","presented_context":"Context: The old lady thinks that there are not enough parking spots near the shopping mall.","presented_target":"<b>Who spots near the shopping parking thinks that there are not enough mall?<\/b>"},{"item":145,"block":6,"list":2,"condition":"UNGRAM","Context":"The dog trainer believes that the animal shelter should remain open during Thanksgiving. ","Target":"Who during believes open Thanksgiving that the shelter should animal remain?","presented_context":"Context: The dog trainer believes that the animal shelter should remain open during Thanksgiving. ","presented_target":"<b>Who during believes open Thanksgiving that the shelter should animal remain?<\/b>"},{"item":122,"block":7,"list":2,"condition":"UNGRAM","Context":"The police officer wonders whether the bystanders witnessed the terrifying incident.","Target":"Who whether incident witnessed wonders the bystanders the terrible?","presented_context":"Context: The police officer wonders whether the bystanders witnessed the terrifying incident.","presented_target":"<b>Who whether incident witnessed wonders the bystanders the terrible?<\/b>"},{"item":123,"block":8,"list":2,"condition":"UNGRAM","Context":"The waiter recommended the new cocktail to the guests who sat on the patio.","Target":"What patio the on sat who guests waiter did the recommend to?","presented_context":"Context: The waiter recommended the new cocktail to the guests who sat on the patio.","presented_target":"<b>What patio the on sat who guests waiter did the recommend to?<\/b>"},{"item":124,"block":9,"list":2,"condition":"UNGRAM","Context":"The reporter thinks that the politician will give a speech at the fundraising event.","Target":"Who event fundraising at thinks the that politician the a speech will give?","presented_context":"Context: The reporter thinks that the politician will give a speech at the fundraising event.","presented_target":"<b>Who event fundraising at thinks the that politician the a speech will give?<\/b>"},{"item":125,"block":10,"list":2,"condition":"UNGRAM","Context":"The tourist wondered whether the art exhibition at the city musuem was worth going to.","Target":"Who worth to going is whether  the art  exhibition wondered city the musuem at?","presented_context":"Context: The tourist wondered whether the art exhibition at the city musuem was worth going to.","presented_target":"<b>Who worth to going is whether  the art  exhibition wondered city the musuem at?<\/b>"},{"item":126,"block":11,"list":2,"condition":"UNGRAM","Context":"The cook believes the claim that the restaurant did not pass the health inspection.","Target":"Who the inspection the did not  restaurant pass health believes the claim that?","presented_context":"Context: The cook believes the claim that the restaurant did not pass the health inspection.","presented_target":"<b>Who the inspection the did not  restaurant pass health believes the claim that?<\/b>"},{"item":127,"block":12,"list":2,"condition":"UNGRAM","Context":"The dentist thinks that the senator should vote for the proposed health care reform.","Target":"Who senator proposed should thinks the that vote health care reform for the?","presented_context":"Context: The dentist thinks that the senator should vote for the proposed health care reform.","presented_target":"<b>Who senator proposed should thinks the that vote health care reform for the?<\/b>"},{"item":128,"block":13,"list":2,"condition":"UNGRAM","Context":"The guitarist wonders whether the drummer will come to the rehearsal.","Target":"Who to the wonders rehearsal the whether drummer come will?","presented_context":"Context: The guitarist wonders whether the drummer will come to the rehearsal.","presented_target":"<b>Who to the wonders rehearsal the whether drummer come will?<\/b>"},{"item":129,"block":14,"list":2,"condition":"UNGRAM","Context":"The sheriff believes the claim that the kidnapper is hiding in the mall.","Target":"Who that the in mall the kidnapper is hiding believes the claim?","presented_context":"Context: The sheriff believes the claim that the kidnapper is hiding in the mall.","presented_target":"<b>Who that the in mall the kidnapper is hiding believes the claim?<\/b>"},{"item":130,"block":15,"list":2,"condition":"UNGRAM","Context":"The nuclear physicist thinks that the new nuclear power plant is extremely safe.","Target":"Who safe thinks plant new that the is extremely power nuclear?","presented_context":"Context: The nuclear physicist thinks that the new nuclear power plant is extremely safe.","presented_target":"<b>Who safe thinks plant new that the is extremely power nuclear?<\/b>"},{"item":116,"block":1,"list":3,"condition":"UNGRAM","Context":"The city officials are trying to find out the source of the virus outbreak.","Target":"What out find to trying officials are the city?","presented_context":"Context: The city officials are trying to find out the source of the virus outbreak.","presented_target":"<b>What out find to trying officials are the city?<\/b>"},{"item":117,"block":2,"list":3,"condition":"UNGRAM","Context":"The software engineer thinks that there is a bug in the system.","Target":"Who there is a the bug  system in  thinks that?","presented_context":"Context: The software engineer thinks that there is a bug in the system.","presented_target":"<b>Who there is a the bug  system in  thinks that?<\/b>"},{"item":118,"block":3,"list":3,"condition":"UNGRAM","Context":"The presidential candidate acknowledged the possibility that there might be an error in the poll result. ","Target":"Who result poll the in error an be might there that acknowledged?","presented_context":"Context: The presidential candidate acknowledged the possibility that there might be an error in the poll result. ","presented_target":"<b>Who result poll the in error an be might there that acknowledged?<\/b>"},{"item":119,"block":4,"list":3,"condition":"UNGRAM","Context":"The trainers believe the claim that the team needs a more sophisticated training regimen.","Target":"What team do needs trainers the that believe the?","presented_context":"Context: The trainers believe the claim that the team needs a more sophisticated training regimen.","presented_target":"<b>What team do needs trainers the that believe the?<\/b>"},{"item":120,"block":5,"list":3,"condition":"UNGRAM","Context":"The analyst wonders whether the level of competition on college sports has become fiercer.","Target":"Who fiercer whether competition sports become the college level in has wonders?","presented_context":"Context: The analyst wonders whether the level of competition on college sports has become fiercer.","presented_target":"<b>Who fiercer whether competition sports become the college level in has wonders?<\/b>"},{"item":121,"block":6,"list":3,"condition":"UNGRAM","Context":"The children who were malnourished recovered from the flu.","Target":"What from malnourished were recover did the children who?","presented_context":"Context: The children who were malnourished recovered from the flu.","presented_target":"<b>What from malnourished were recover did the children who?<\/b>"},{"item":122,"block":7,"list":3,"condition":"UNGRAM","Context":"The police officer wonders whether the bystanders witnessed the terrifying incident.","Target":"Who whether incident witnessed wonders the bystanders the terrible?","presented_context":"Context: The police officer wonders whether the bystanders witnessed the terrifying incident.","presented_target":"<b>Who whether incident witnessed wonders the bystanders the terrible?<\/b>"},{"item":123,"block":8,"list":3,"condition":"UNGRAM","Context":"The waiter recommended the new cocktail to the guests who sat on the patio.","Target":"What patio the on sat who guests waiter did the recommend to?","presented_context":"Context: The waiter recommended the new cocktail to the guests who sat on the patio.","presented_target":"<b>What patio the on sat who guests waiter did the recommend to?<\/b>"},{"item":124,"block":9,"list":3,"condition":"UNGRAM","Context":"The reporter thinks that the politician will give a speech at the fundraising event.","Target":"Who event fundraising at thinks the that politician the a speech will give?","presented_context":"Context: The reporter thinks that the politician will give a speech at the fundraising event.","presented_target":"<b>Who event fundraising at thinks the that politician the a speech will give?<\/b>"},{"item":125,"block":10,"list":3,"condition":"UNGRAM","Context":"The tourist wondered whether the art exhibition at the city musuem was worth going to.","Target":"Who worth to going is whether  the art  exhibition wondered city the musuem at?","presented_context":"Context: The tourist wondered whether the art exhibition at the city musuem was worth going to.","presented_target":"<b>Who worth to going is whether  the art  exhibition wondered city the musuem at?<\/b>"},{"item":126,"block":11,"list":3,"condition":"UNGRAM","Context":"The cook believes the claim that the restaurant did not pass the health inspection.","Target":"Who the inspection the did not  restaurant pass health believes the claim that?","presented_context":"Context: The cook believes the claim that the restaurant did not pass the health inspection.","presented_target":"<b>Who the inspection the did not  restaurant pass health believes the claim that?<\/b>"},{"item":127,"block":12,"list":3,"condition":"UNGRAM","Context":"The dentist thinks that the senator should vote for the proposed health care reform.","Target":"Who senator proposed should thinks the that vote health care reform for the?","presented_context":"Context: The dentist thinks that the senator should vote for the proposed health care reform.","presented_target":"<b>Who senator proposed should thinks the that vote health care reform for the?<\/b>"},{"item":128,"block":13,"list":3,"condition":"UNGRAM","Context":"The guitarist wonders whether the drummer will come to the rehearsal.","Target":"Who to the wonders rehearsal the whether drummer come will?","presented_context":"Context: The guitarist wonders whether the drummer will come to the rehearsal.","presented_target":"<b>Who to the wonders rehearsal the whether drummer come will?<\/b>"},{"item":129,"block":14,"list":3,"condition":"UNGRAM","Context":"The sheriff believes the claim that the kidnapper is hiding in the mall.","Target":"Who that the in mall the kidnapper is hiding believes the claim?","presented_context":"Context: The sheriff believes the claim that the kidnapper is hiding in the mall.","presented_target":"<b>Who that the in mall the kidnapper is hiding believes the claim?<\/b>"},{"item":130,"block":15,"list":3,"condition":"UNGRAM","Context":"The nuclear physicist thinks that the new nuclear power plant is extremely safe.","Target":"Who safe thinks plant new that the is extremely power nuclear?","presented_context":"Context: The nuclear physicist thinks that the new nuclear power plant is extremely safe.","presented_target":"<b>Who safe thinks plant new that the is extremely power nuclear?<\/b>"}];


exposure_stimuli = all_stimuli.filter(function (e){
  return e.list == 1;
});
test_stimuli = all_stimuli.filter(function (e){
  return e.list == 2;
});

var shuffle = function (array) {

	var currentIndex = array.length;
	var temporaryValue, randomIndex;

	// While there remain elements to shuffle...
	while (0 !== currentIndex) {
		// Pick a remaining element...
		randomIndex = Math.floor(Math.random() * currentIndex);
		currentIndex -= 1;

		// And swap it with the current element.
		temporaryValue = array[currentIndex];
		array[currentIndex] = array[randomIndex];
		array[randomIndex] = temporaryValue;
	}

	return array;

};

exp_cond = shuffle(["CNPC","SUBJ","WH"])[0];
test_match_cond = shuffle(["match", "mismatch"])[0];


block_1 = exposure_stimuli.filter(function (e) {
  return e.block == 1;
});
block_2 = exposure_stimuli.filter(function (e) {
  return e.block == 2;
});
block_3 = exposure_stimuli.filter(function (e) {
  return e.block == 3;
});
block_4 = exposure_stimuli.filter(function (e) {
  return e.block == 4;
});
block_5 = exposure_stimuli.filter(function (e) {
  return e.block == 5;
});
block_6 = exposure_stimuli.filter(function (e) {
  return e.block == 6;
});
block_7 = exposure_stimuli.filter(function (e) {
  return e.block == 7;
});
block_8 = exposure_stimuli.filter(function (e) {
  return e.block == 8;
});
block_9 = exposure_stimuli.filter(function (e) {
  return e.block == 9;
});
block_10 = exposure_stimuli.filter(function (e) {
  return e.block == 10;
});
block_11 = exposure_stimuli.filter(function (e) {
  return e.block == 11;
});
block_12 = exposure_stimuli.filter(function (e) {
  return e.block == 12;
});
block_13 = exposure_stimuli.filter(function (e) {
  return e.block == 13;
});
block_14 = exposure_stimuli.filter(function (e) {
  return e.block == 14;
});
block_15 = exposure_stimuli.filter(function (e) {
  return e.block == 15;
});

test_1 = test_stimuli.filter(function (e) {
  return e.block == 1;
});
test_2 = test_stimuli.filter(function (e) {
  return e.block == 2;
});
test_3 = test_stimuli.filter(function (e) {
  return e.block == 3;
});
test_4 = test_stimuli.filter(function (e) {
  return e.block == 4;
});
test_5 = test_stimuli.filter(function (e) {
  return e.block == 5;
});
test_6 = test_stimuli.filter(function (e) {
  return e.block == 6;
});



block_1 = shuffle(block_1);
block_2 = shuffle(block_2);
block_3 = shuffle(block_3);
block_4 = shuffle(block_4);
block_5 = shuffle(block_5);
block_6 = shuffle(block_6);
block_7 = shuffle(block_7);
block_8 = shuffle(block_8);
block_9 = shuffle(block_9);
block_10 = shuffle(block_10);
block_11 = shuffle(block_11);
block_12 = shuffle(block_12);
block_13 = shuffle(block_13);
block_14 = shuffle(block_14);
block_15 = shuffle(block_15);
test1 = shuffle(test_1);
test2 = shuffle(test_2);
test3 = shuffle(test_3);
test4 = shuffle(test_4);
test5 = shuffle(test_5);
test6 = shuffle(test_6);

shuffled_blocks = shuffle([block_1, block_2, block_3, block_4, block_5, block_6, block_7, block_8, block_9, block_10, block_11, block_12, block_13, block_14, block_15]);
shuffled_tests = shuffle([test_1, test_2, test_3, test_4, test_5, test_6]);
//shuffled_blocks = shuffle([block_1]);
name_list = shuffle(["Gregory", "Emily", "Jessy", "Thomas"]);

for (var i = 0; i < 15; i++){
  for (var j in shuffled_blocks[i]){

    shuffled_blocks[i][j]["new_block_sequence"] = i+1;
    shuffled_blocks[i][j]["island_tested"] = exp_cond;
    shuffled_blocks[i][j]["phase"] = "exposure"
    shuffled_blocks[i][j]["test_match_cond"] = test_match_cond;
    if (shuffled_blocks[i][j]["condition"] == exp_cond){
        shuffled_blocks[i][j]["name"] = name_list[0];}
    else if (shuffled_blocks[i][j]["condition"] == "FILL"){
        shuffled_blocks[i][j]["name"] = name_list[1]}
    else if (shuffled_blocks[i][j]["condition"] == "UNGRAM"){
        shuffled_blocks[i][j]["name"] = "Iron-Head";}
    else {
      shuffled_blocks[i][j]["name"] = "delete";}
      
  };
};

for (var i = 0; i < 6; i++){
  for (var j in shuffled_tests[i]){

    shuffled_tests[i][j]["new_block_sequence"] = i+101;
    shuffled_tests[i][j]["phase"] = "test"
    shuffled_tests[i][j]["island_tested"] = exp_cond;
    shuffled_tests[i][j]["test_match_cond"] = test_match_cond;
      if (test_match_cond == "mismatch"){
          
        if (shuffled_tests[i][j]["condition"] == exp_cond){
            shuffled_tests[i][j]["name"] = name_list[1];}
        else if (shuffled_tests[i][j]["condition"] == "FILL"){
            shuffled_tests[i][j]["name"] = name_list[0]}
        else if (shuffled_tests[i][j]["condition"] == "UNGRAM"){
            shuffled_tests[i][j]["name"] = "Iron-Head";}
        else {
          shuffled_tests[i][j]["name"] = "delete";}
          
      } 
      else {
      
        if (shuffled_tests[i][j]["condition"] == exp_cond){
          shuffled_tests[i][j]["name"] = name_list[0];}
      else if (shuffled_tests[i][j]["condition"] == "FILL"){
          shuffled_tests[i][j]["name"] = name_list[1]}
      else if (shuffled_tests[i][j]["condition"] == "UNGRAM"){
          shuffled_tests[i][j]["name"] = "Iron-Head";}
      else {
        shuffled_tests[i][j]["name"] = "delete";}


      };

    };
};

latin_squared = [shuffled_blocks, shuffled_tests].flat().flat();
console.log(latin_squared)
latin_squared = latin_squared.filter(i => i["name"] != "delete");

//shuffle name-condition coorelation

function resetSelectElement(selectElement) {
  var options = selectElement.options;

  // Look for a default selected option
  for (var i=0, iLen=options.length; i<iLen; i++) {

      if (options[i].defaultSelected) {
          selectElement.selectedIndex = i;
          return;
      }
  }
  // If no option is the default, select first or none as appropriate
  selectElement.selectedIndex = 0; // or -1 for no option selected
}


function make_slides(f) {
  var slides = {};  
  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.single_trial = slide({
    name: "single_trial",
    start: function() {
      $(".err").hide();
      $(".display_condition").html("You are in " + exp.condition + ".");
    },
    button : function() {
      response = $("#text_response").val();
      if (response.length == 0) {
        $(".err").show();
      } else {
        exp.data_trials.push({
          "trial_type" : "single_trial",
          "response" : response
        });
        exp.go(); //make sure this is at the *end*, after you log your data
      }
    },
  });

  slides.practice_slider = slide({
    name : "practice_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : [{"a": 1}],
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      var name = 'John';
      $(".err").hide();
      $(".errgood").hide();
      $(".errbad").hide();
      $(".target").hide();
      $(".slider_table").hide();
      $(".button_2").hide()
      this.stim = stim;
    
      var init_image = '<img src="images/'+ name + '.png" style="height:150px" class="center">';
      $(".image").html(init_image)
      $(".figure_intro").html("This is <b>"+name+"<\/b.")
      $(".context").html("Context: The boy saw an apple on the table.");
      $(".target").html(name +" asks: <b> What did the boy see on the table? <\/b>");
      $(".button_1").html("Click here to see what <b>" +name+ "<\/b> asks about the context.")
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
      exp.first_response_wrong = 0;
      exp.first_response_value = null;
      exp.attempts = 0;
    },
    button_1 : function() {
      $(".target").show();
      $(".slider_table").show();
      $(".button_2").show()
      $(".button_1").hide()
    },
    button_2 : function() {
 
      if (exp.sliderPost == null) {
        $(".err").show();

      } 
      else if (exp.sliderPost < 0.5) {
        exp.first_response_wrong = 1;
        exp.first_response_value =exp.sliderPost;
        exp.attempts = exp.attempts + 1;
        $(".errgood").show();
      }
      else {
        this.log_responses();
        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },
    init_sliders : function() {
      utils.make_slider("#practice_slider_1", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "first_response_value": exp.first_response_value,
        "wrong_attempts": exp.attempts,
        "condition" : "practice_good",
        "block_sequence": "practice",
        "item_number": "practice_good",
        "list_number": "practice",
        "trial_sequence_total": 0
      });

    }
  });

  slides.post_practice_1 = slide({
    name : "post_practice_1",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.practice_slider_bad = slide({
    name : "practice_slider_bad",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : [1],

  
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      var name = 'Mary';
      $(".button_1").show()
      $(".err").hide();
      $(".errgood").hide();
      $(".errbad").hide();
      $(".target").hide();
      $(".slider_table").hide();
      $(".button_2").hide()
      this.stim = stim;
    
      var init_image = '<img src="images/'+ name + '.png" style="height:150px" class="center">';
      $(".image").html(init_image)
      $(".figure_intro").html("This is <b>"+name+"<\/b.")
      $(".context").html("Context: The girl slept under the bed.");
      $(".target").html(name +" asks: <b> Who the bed was slept under? <\/b>")
      $(".button_1").html("Click here to see what <b>" +name+ "<\/b> asks about the context.")
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
      exp.first_response_wrong = 0;
      exp.first_response_value = null;
      exp.attempts = 0;
    },
    button_1 : function() {
      $(".target").show();
      $(".slider_table").show();
      $(".button_2").show()
      $(".button_1").hide()
    },
    button_2 : function() {
 
      if (exp.sliderPost == null) {
        $(".err").show();
        
      } 
      else if (exp.sliderPost > 0.5) {
        exp.first_response_wrong = 1;
        exp.first_response_value = exp.sliderPost;
        exp.attempts = exp.attempts + 1;
        $(".errbad").show();
      }
      else {
        this.log_responses();
        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },
    init_sliders : function() {
      utils.make_slider("#practice_slider_2", function(event, ui) {
        exp.sliderPost = ui.value;
        
      });
    },
    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "first_response_value": exp.first_response_value,
        "wrong_attempts": exp.attempts,
        "condition" : "practice_bad",
        "block_sequence": "practice",
        "item_number": "practice_bad",
        "list_number": "practice",
        "trial_sequence_total": 0
      });

    }
  });

  slides.post_practice_2 = slide({
    name : "post_practice_2",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.speaker_intro1 = slide({
    name : "speaker_intro1",
    //this gets run only at the beginning of the block
    start : function(){
      var init_image = '<img src="images/'+ name_list[0] + '.png" style="height:150px" class="center">';

      $(".speaker_intro_line").html("Let me introduce you to "+name_list[0]+", "+name_list[1]+", and Iron-Head! They will be asking some questions during this experiment, and you will be rating how acceptable their questions sound!")
      $(".speaker_image").html(init_image)
      $(".specific_speaker_intro_line").html("This is "+name_list[0]+"!")
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });

  slides.speaker_intro2 = slide({
    name : "speaker_intro2",
    //this gets run only at the beginning of the block
    start : function(){
      var init_image = '<img src="images/'+ name_list[1] + '.png" style="height:150px" class="center">';

      $(".speaker_intro_line").html("Let me introduce you to "+name_list[0]+", "+name_list[1]+", and Iron-Head! They will be asking some questions during this experiment, and you will be rating how acceptable their questions sound!")
      $(".speaker_image").html(init_image)
      $(".specific_speaker_intro_line").html("This is "+name_list[1]+"!")
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });

  slides.speaker_intro5 = slide({
    name : "speaker_intro5",
    //this gets run only at the beginning of the block
    start : function(){
      var init_image = '<img src="images/Iron-Head.png" style="height:150px" class="center">';

      $(".speaker_intro_line").html("Let me introduce you to "+name_list[0]+", "+name_list[1]+", and Iron-Head! They will be asking some questions during this experiment, and you will be rating how acceptable their questions sound!")
      $(".speaker_image").html(init_image)
      $(".specific_speaker_intro_line").html("This is <b>Iron-Head<\/b>! Iron-Head is a robot!")
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });

  
  slides.speaker_intro_final = slide({
    name : "speaker_intro_final",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });


  slides.last_reminder = slide({
    name : "last_reminder",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
    
  });

  slides.one_slider = slide({
    name : "one_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : latin_squared,
    
    
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      $(".target").hide();
      $(".slider_table").hide();
      $(".button_2").hide()
      $(".button_3").hide()
      $(".speaker_question").hide()
      $(".context").show();
      $(".button_1").show();
      $(".image").show();
      $(".figure_intro").show();
  
      this.stim = stim; //I like to store this information in the slide so I can record it later.
      $(".context").html(stim.presented_context);
      var init_image = '<img src="images/'+ stim.name + '.png" style="height:150px" class="center">';
      $(".image").html(init_image)
      $(".button_1").html("Click here to see what <b>"+ stim.name + "<\/b> asks about the context.")
      $(".figure_intro").html("This is <b>"+stim.name+"<\/b.")
      $(".target").html(stim.name + " asks: " + stim.presented_target);

      this.init_sliders()
      exp.sliderPost = null; //erase current slider value
      resetSelectElement(comp_q);
    },
    button_1 : function() {
      $(".target").show();
      $(".button_1").hide();
      $(".slider_table").show();
      if (Math.random() > 0.7){
        $(".button_3").show()
      }
      else{
      $(".button_2").show()
      }
    },

    button_3 : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      }
      else {
      $(".target").hide();
      $(".context").hide();
      $(".slider_table").hide();
      $(".button_1").hide();
      $(".image").hide();
      $(".figure_intro").hide()
      $(".speaker_question").show();
      $(".button_3").hide()
      $(".button_2").show()
      }
    },

    button_2 : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      }
      else {
        this.log_responses();
        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    
  },
    
    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "island_tested": this.stim.island_tested,
        "test_match_cond":this.stim.test_match_cond,
        "condition" : this.stim.condition,
        "block_sequence": this.stim.new_block_sequence,
        "item_number": this.stim.item,
        "list_number": this.stim.list,
        "trial_sequence_total": order,
        "speaker_identity":this.stim.name,
        "comp_answer": $("#comp_q").val(),
        "phase": this.stim.phase
      });
      order = order + 1;
    }
  });



  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];
  //exp.condition = _.sample(["condition 1", "condition 2"]); //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "practice_slider", "post_practice_1", "practice_slider_bad", "post_practice_2", "speaker_intro1","speaker_intro2","speaker_intro5","speaker_intro_final", "last_reminder", 'one_slider', 'subj_info', 'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}

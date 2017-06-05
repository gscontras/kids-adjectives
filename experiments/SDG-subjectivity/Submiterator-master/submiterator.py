#!/usr/bin/env python

import os, json, re, csv, argparse

def main():
    parser = argparse.ArgumentParser(description='Interface with MTurk.')
    parser.add_argument("subcommand", choices=['posthit', 'getresults', 'reformat',
        "preparefiles", "anonymize"],
        type=str, action="store",
        help="choose a specific subcommand.")
    parser.add_argument("nameofexperimentfiles", metavar="label", type=str, nargs="+",
        help="you must have at least one label that corresponds to the " +
        "experiment you want to work with. each experiment has a unique label. " +
        "this will be the beginning of the name of the config file (everything " +
        "before the dot). [label].config.")
    # parser.add_argument("--live", dest='is_live', action='store_const',
    #   const=True, default=False, help="interface with live site (defaults to sandbox)")
    # parser.add_argument("--posthit", help="post a hit to mturk. requires label")
    # parser.add_argument("--getresults", help="get results from mturk. requires label")
    # parser.add_argument("--reformat")
    # parser.add_argument("--preparefiles")
    # parser.add_argument("--anonymize")
    # parser.add_argument('integers', metavar='N', type=int, nargs='+',
    #                    help='an integer for the accumulator')
    # parser.add_argument('--sum', dest='accumulate', action='store_const',
    #                    const=sum, default=max,
    #                    help='sum the integers (default: find the max)')

    # parser = argparse.ArgumentParser(description='Interface with MTurk.')
    # parser.add_argument('integers', metavar='N', type=int, nargs='+',
    #                    help='an integer for the accumulator')
    # parser.add_argument('--sum', dest='accumulate', action='store_const',
    #                    const=sum, default=max,
    #                    help='sum the integers (default: find the max)')

    args = parser.parse_args()
    # print args.accumulate(args.integers)

    subcommand = vars(args)["subcommand"]
    labels = vars(args)["nameofexperimentfiles"]

    for label in labels:
        if subcommand == "posthit":
            prepare(label)
            posthit(label)
        elif subcommand == "getresults":
            getresults(label)
            anonymize(label + ".results")
        elif subcommand == "reformat":
            try:
                try:
                    reformat(label + ".results")
                except IOError:
                    reformat(label + "_anonymized.results")
            except IOError:
                print ("\nWARNING: cannot find file `" + label +
                ".results` or its anonymized version `" + label +
                "_anonymized.results`.\nSKIPPING!\n")
        elif subcommand == "preparefiles":
            prepare(label)
        elif subcommand == "anonymize":
            anonymize(label + ".results")

def prepare(nameofexperimentfiles, output_dir=""):

    config_file = nameofexperimentfiles + ".config"

    settings = open(config_file, 'r')
    json_string = settings.read()
    settings.close()
    dict = json.loads(re.sub("\n", "", json_string))

    locationofCLT = os.environ['MTURK_CMD_HOME']
    # dict={}
    # for line in lines:
    #     if not (line == "\n" or line == ""):
    #         while (line[0] == "\n" or line[0] == "\t" or line[0] == " "):
    #             line = line[1:]
    #         if not (line[0:3] == "###"):
    #             x=line.split("::")
    #             key = x[0]
    #             possvalues = x[1].split("###")
    #             value = possvalues[0]
    #             while (key[0] == "\n" or key[0] == "\t" or key[0] == " "):
    #                 key = key[1:]
    #             while (key[-1] == "\n" or key[-1] == "\t" or key[-1] == " "):
    #                 key = key[:-1]
    #             while (value[0] == "\n" or value[0] == "\t" or value[0] == " "):
    #                 value = value[1:]
    #             while (value[-1] == "\n" or value[-1] == "\t" or value[-1] == " "):
    #                 value = value[:-1]
    #             dict[x[0]] = value

    if not os.path.exists(locationofCLT) or locationofCLT == '/':
        raise Exception("Error: please set your 'MTURK_CMD_HOME' environment variable to your AWS directory.")

    if dict["rewriteProperties"] == "yes":
        old_properties_file = open(locationofCLT + "/bin/mturk.properties", 'r').readlines()
        backup = open(locationofCLT + "/bin/mturk.properties.backup", 'w')
        for line in old_properties_file:
            backup.write(line + '\n')
        backup.close()
        new_properties_file = open(locationofCLT + "/bin/mturk.properties", 'w')
        if (dict["liveHIT"] == "yes"):
            for line in old_properties_file:
                if "://mechanicalturk.sandbox.amazonaws.com/?Service=AWSMechanicalTurkRequester" in line:
                    new_properties_file.write("# service_url=https://mechanicalturk.sandbox.amazonaws.com/?Service=AWSMechanicalTurkRequester\n")
                elif "://mechanicalturk.amazonaws.com/?Service=AWSMechanicalTurkRequester" in line:
                     new_properties_file.write("service_url=https://mechanicalturk.amazonaws.com/?Service=AWSMechanicalTurkRequester\n")
                else:
                    new_properties_file.write(line)
        else:
            for line in old_properties_file:
                if "://mechanicalturk.sandbox.amazonaws.com/?Service=AWSMechanicalTurkRequester" in line:
                    new_properties_file.write("service_url=https://mechanicalturk.sandbox.amazonaws.com/?Service=AWSMechanicalTurkRequester\n")
                elif "://mechanicalturk.amazonaws.com/?Service=AWSMechanicalTurkRequester" in line:
                    new_properties_file.write("# service_url=https://mechanicalturk.amazonaws.com/?Service=AWSMechanicalTurkRequester\n")
                else:
                    new_properties_file.write(line)
        new_properties_file.close()
        print "Old mturk.properties file backed up at " + locationofCLT + "/bin/mturk.properties.backup" 

    # write the .question file, which tells MTurk where to find your external HIT.
    question = open(output_dir + nameofexperimentfiles + ".question", 'w')
    question.write("<?xml version='1.0'?><ExternalQuestion xmlns='http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2006-07-14/ExternalQuestion.xsd'><ExternalURL>" + dict["experimentURL"] + "</ExternalURL><FrameHeight>"+ dict["frameheight"] +"</FrameHeight></ExternalQuestion>")
    question.close()

    #write the .properties file.
    properties = open(output_dir + nameofexperimentfiles + ".properties", 'w')
    properties.write("title: " + dict["title"] + "\ndescription: " + dict["description"] + "\nkeywords: " + dict["keywords"] + "\nreward: " + dict["reward"] + "\nassignments: " + dict["numberofassignments"] + "\nannotation: ${condition}\nassignmentduration:" + dict["assignmentduration"] + "\nhitlifetime:" + dict["hitlifetime"] + "\nautoapprovaldelay:" + dict["autoapprovaldelay"])
    if (dict["USonly?"] == "y" or dict["USonly?"] == "Y" or dict["USonly?"] == "yes" or dict["USonly?"] == "Yes" or dict["USonly?"] == "true" or dict["USonly?"] == "True" or dict["USonly?"] == "T" or dict["USonly?"] == "1"):
        properties.write("\nqualification.1:00000000000000000071\nqualification.comparator.1:EqualTo\nqualification.locale.1:US\nqualification.private.1:false")
    if (dict["minPercentPreviousHITsApproved"] != "none"):
        properties.write("\nqualification.2:000000000000000000L0\nqualification.comparator.2:GreaterThanOrEqualTo\nqualification.value.2:" + dict["minPercentPreviousHITsApproved"] + "\nqualification.private.2:false")
    properties.close()

    #write the .input file. "conditions::" in the file experiment-settings.txt can be followed by any number of condition names, separated by a comma.
    input = open(output_dir + nameofexperimentfiles + ".input", 'w')
    input.write("condition\n")
    num = 1
    conditions = dict["conditions"]
    conditionlist = conditions.split(",")
    for x in conditionlist:
        input.write(str(num) + " " + x + " \n")
        num = num + 1
    input.close()

def reformat(mturk_data_file, workers={}):

  mturk_tag = mturk_data_file[:-8]
  output_data_file_label = mturk_tag

  def clean_text(text):
    return text

  def write_2_by_2(data, filename, sep="\t"):
    w = open(filename, "w")
    w.write("\n".join(map(lambda x: sep.join(x), data)))
    w.close()

  # workers = json.loads(re.sub("'", '"', re.sub(" u'", " '", sys.argv[2])))

  def symb(workerid):
    if workerid in workers:
      return workers[workerid]
    else:
      id_number = str(len(workers))
      workers[workerid] = id_number
      return id_number

  def get_column_labels(data_type):
    new_column_labels_from_json = set()
    with open(mturk_data_file, 'rb') as csvfile:
      header_labels = []
      header = True
      mturk_reader = csv.reader(csvfile, delimiter='\t', quotechar='"')
      for row in mturk_reader:
        if header:
          header = False
          header_labels = row
          if data_type == "mturk":
            return [x for x in header_labels if (not x in ["Answer.trials", "Answer.subject_information", "Answer.check_trials", "Answer.system"])]
        else:
          for i in range(len(row)):
            elem = re.sub("'", "", row[i])
            label = header_labels[i]
            if label == "Answer." + data_type:
              if label == "Answer.trials" or label == "Answer.catch_trials":
                this_is_hacky = "{\"this_is_hacky\":" + elem + "}"
                trials = json.loads(this_is_hacky)["this_is_hacky"]
                for trial in trials:
                  new_column_labels_from_json.update(trial.keys())
              elif label == "Answer.subject_information":
                data = json.loads(elem)
                new_column_labels_from_json.update(data.keys())
              elif label == "Answer.system":
                data = json.loads(elem)
                new_column_labels_from_json.update(data.keys())
    return list(new_column_labels_from_json)

  def make_tsv(data_type):
    new_column_labels = get_column_labels(data_type)
    mturk_output_column_names = []
    output_rows = [["workerid"] + new_column_labels]
    with open(mturk_data_file, 'rb') as csvfile:
      header_labels = []
      header = True
      mturk_reader = csv.reader(csvfile, delimiter='\t', quotechar='"')
      for row in mturk_reader:
        if header:
          header = False
          header_labels = row
          mturk_output_column_names = [x for x in header_labels if (not x in ["Answer.trials", "Answer.subject_information", "Answer.check_trials", "Answer.system"])]
          #output_rows.append(output_column_names)
        else:
          subject_level_data = {}
          trial_level_data = {}
          for key in new_column_labels:
            trial_level_data[key] = []
          for i in range(len(row)):
            elem = re.sub("'", "&quotechar", row[i])
            label = header_labels[i]
            if label == "Answer." + data_type:
              if label == "Answer.trials" or label == "Answer.catch_trials":
                this_is_hacky = "{\"this_is_hacky\":" + elem + "}"
                trials = json.loads(this_is_hacky)["this_is_hacky"]
                for trial in trials:
                  for key in new_column_labels:
                    if key in trial.keys():
                      trial_level_data[key].append(str(trial[key]))
                    else:
                      trial_level_data[key].append("NA")
              else:
                if label == "Answer.subject_information":
                  data = json.loads(elem)
                elif label == "Answer.system":
                  data = json.loads(elem)
                for key in new_column_labels:
                  if key in data.keys():
                    subject_level_data[key] = str(data[key])
                  else:
                    subject_level_data[key] = "NA"
            elif label == "workerid":
              elem = symb(elem)
              subject_level_data["workerid"] = elem
            else:
              subject_level_data[label] = str(elem)
          if len(trial_level_data.keys()) > 0:
            ntrials = len(trial_level_data[trial_level_data.keys()[0]])
          else:
            ntrials = 0
          if ntrials > 0:
            #print ntrials
            for i in range(ntrials):
              output_row = []
              # for key in mturk_output_column_names:
              #   output_row.append(subject_level_data[key])
              output_row.append(subject_level_data["workerid"])
              for key in new_column_labels:
                output_row.append(trial_level_data[key][i])
              output_rows.append(output_row)
          else:
            output_row = []
            # for key in mturk_output_column_names:
            #   output_row.append(subject_level_data[key])
            output_row.append(subject_level_data["workerid"])
            #print data_type
            for key in new_column_labels:
              output_row.append(subject_level_data[key])
            output_rows.append(output_row)
    write_2_by_2(output_rows, output_data_file_label + "-" + data_type + ".tsv")
    return [[clean_text(elem) for elem in row] for row in output_rows]


  # trial_columns = get_column_labels("trial")
  # check_trial_columns = get_column_labels("check_trial")
  # subject_information_columns = get_column_labels("subject_information")
  # system_columns = get_column_labels("system")

  def make_full_tsv():
    trials = make_tsv("trials")
    catch_trials = make_tsv("catch_trials")
    subject_information = make_tsv("subject_information")
    system = make_tsv("system")
    mturk = make_tsv("mturk")
    big_rows = [
      trials[0] + subject_information[0][1:] + system[0][1:] + mturk[0][1:]
    ]
    workerids = [row[0] for row in mturk][1:]
    for workerid in workerids:
      small_trials = [row for row in trials if row[0] == workerid] #has workerid
      small_catch_trials = [row for row in catch_trials if row[0] == workerid] #has workerid
      small_subject_information = [row[1:] for row in subject_information if row[0] == workerid][0]
      small_system = [row[1:] for row in system if row[0] == workerid][0]
      small_mturk = [row[1:] for row in mturk if row[0] == workerid][0]
      ntrials = len(small_trials)
      if ntrials > 0:
        for trial in small_trials:
          big_row = trial + small_subject_information + small_system + small_mturk
          big_rows.append(big_row)
    write_2_by_2(big_rows, output_data_file_label + ".tsv")

  make_full_tsv()

  print workers

def anonymize(original_data_filename):
    workers = {}
    def symb(workerid):
        if workerid in workers:
            return workers[workerid]
        else:
            id_number = str(len(workers))
            workers[workerid] = id_number
            return id_number

    new_data_filename = original_data_filename.split(".results")[0] + "_anonymized.results"
    new_rows = []

    with open(original_data_filename, "rb") as csvfile:
        csvreader = csv.reader(csvfile, delimiter="\t")
        is_header = True
        workerIndex = 0
        for row in csvreader:
            if is_header:
                workerIndex = row.index("workerid")
                is_header = False
            else:
                row[workerIndex] = symb(row[workerIndex])
            new_rows.append("\t".join(row))

    w = open(new_data_filename, "w")
    w.write("\n".join(new_rows))
    w.close()

    print workers

def posthit(label):
    os.system(
        """
        HERE=`pwd`
        cd $MTURK_CMD_HOME/bin

        NAME_OF_EXPERIMENT_FILES=""" + label + """
        label=$HERE/$NAME_OF_EXPERIMENT_FILES
        ./loadHITs.sh -label $label -input $label.input -question $label.question -properties $label.properties -maxhits 1
        """
        )

def getresults(label):
    os.system("""
        HERE=`pwd`
        cd $MTURK_CMD_HOME/bin

        NAME_OF_EXPERIMENT_FILES=""" + label + """
        label=$HERE/$NAME_OF_EXPERIMENT_FILES
        ./getResults.sh -successfile $label.success -outputfile $label.results
        """)

# submiterator --preparefiles label [label ...]

# submiterator --posthit label [label ...]

# # submiterator --sandbox
# # submiterator --livehit

# submiterator --getresults label [label ...]

# submiterator --reformat file [file ...] output_dir

main()
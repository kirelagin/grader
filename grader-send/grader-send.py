#!/usr/bin/env python3

from email.encoders import encode_base64, encode_quopri
from email.header import Header
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.utils import make_msgid
import git
import itertools
import os.path
import re
import subprocess
import sys


def prepare_mail(repo, assignment, user):
  def extract_msgid(commit):
    meta_lines = itertools.takewhile(lambda l: l.strip() != '==', commit.message.splitlines())
    for line in meta_lines:
      if line.startswith('Message-ID'):
        return line.split(':', 1)[1].strip()

  def collect_files(tree, dir_name=''):
    files = {}
    for item in tree.traverse():
      if item.type != 'blob':
        raise ValueError('Found not a blob')
      files[os.path.join(dir_name, item.name)] = item.data_stream
    return files

  commit = repo.refs['graded/{}/{}'.format(assignment, user)].commit
  parent = commit.parents[1]

  msgid = extract_msgid(parent)
  if not msgid: raise ValueError('No Message-Id in the parent')
  files = collect_files(commit.tree)

  msg = MIMEMultipart()
  msg['Subject'] = '{} results'.format(assignment)
  msg['From'] = 'Haskell HW <haskell@au.kir.elagin.me>'
  msg['Reply-To'] = '<haskell@au.kir.elagin.me>'
  to = Header()
  to.append(parent.author.name)
  to.append('<{}>'.format(parent.author.email))
  msg['To'] = to.encode()
  msg['Message-Id'] = make_msgid('grader-result')
  msg['In-Reply-To'] = msgid
  msg['References'] = msgid

  text = MIMEText(commit.message, 'plain', 'utf-8')
  msg.attach(text)

  for (name, content) in files.items():
    atchm = MIMEText(content.read(), 'plain', 'utf-8')
    encode_base64(atchm)
    atchm.add_header('Content-Disposition', 'attachment', filename=name)
    msg.attach(atchm)

  print('Mail to {}/{} (commit {})'.format(assignment, user, commit))
  return msg

if __name__ == '__main__':
  repo = git.Repo('.')

  branch = repo.head.ref
  match = re.match(r'(?:([^/]+)/)?([^/]+)/([^/]+)/([^/]+)', branch.name).groups()
  if match is None: raise ValueError('On a wrong branch')
  origin, kind, asgn, user = match

  msg = prepare_mail(repo, asgn, user)

  process = subprocess.Popen(['ssh', 'blua.kir.elagin.me', '/usr/sbin/sendmail', '-t'], stdin=subprocess.PIPE)
  process.communicate(msg.as_string().encode('utf-8'))

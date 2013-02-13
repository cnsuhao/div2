import csv
import re
import sys
import traceback

from collections import namedtuple
from functools import partial

import atom
from gdata import contacts
from gdata.service import Query
from gdata.contacts.service import ContactsService

def strjoin(c,ll):
    return c.join(v.strip() for v in ll if v.strip())

schema = [
    'first',
    'last',
    'company',
    'title',
    'addr_work_street',
    'addr_work_city',
    'addr_work_state',
    'addr_work_zip',
    'addr_home_street',
    'addr_home_city',
    'addr_home_state',
    'addr_home_zip',
    'addr_other_street',
    'addr_other_city',
    'addr_other_state',
    'addr_other_zip',
    'phone_work1',
    'phone_home',
    'phone_mobile',
    'email1',
    'phone_fax',
    'phone_work2',
    'email2',
    'notes',
    'category'
]

ContactCSV = namedtuple('ContactCSV', ' '.join(schema))
Field = namedtuple('Field', 'text rel')

class Contact(object):
    def __init__(self, info):
        self._info = info
    
        # basic info
        self.name = strjoin(', ', [info.last, info.first])
        self.category = info.category.strip()
        self.company = info.company.strip()
        self.title = info.title.strip()
        
        
        # phones
        num = partial(re.sub, r"(\d{3}).(\d{3}).(\d{4})", r"\1-\2-\3")
        
        self.phones = [
            Field(num(info.phone_work1), contacts.PHONE_WORK),
            Field(num(info.phone_work2), contacts.PHONE_WORK),
            Field(num(info.phone_home), contacts.PHONE_HOME),
            Field(num(info.phone_mobile), contacts.PHONE_MOBILE),
            Field(num(info.phone_fax), contacts.PHONE_FAX)
        ]
        self.phones = [ num for num in self.phones if num.text.strip() ]
        
        # addresses
        self.addresses = [
            Field( strjoin('\n', [ info.addr_work_street, 
                                   strjoin(', ', [ info.addr_work_city, 
                                                   info.addr_work_state, 
                                                   info.addr_work_zip, ]) ]).replace('\r',''),
                   contacts.REL_WORK ),
            Field( strjoin('\n', [ info.addr_home_street, 
                                   strjoin(', ', [ info.addr_home_city, 
                                                   info.addr_home_state, 
                                                   info.addr_home_zip, ]) ]).replace('\r',''),
                   contacts.REL_HOME ),
            Field( strjoin('\n', [ info.addr_other_street, 
                                   strjoin(', ', [ info.addr_other_city, 
                                                   info.addr_other_state, 
                                                   info.addr_other_zip, ]) ]).replace('\r',''),
                   contacts.REL_OTHER )
        ]
        self.addresses = [ addr for addr in self.addresses if addr.text.strip() ]
        
        # email addresses
        self.emails = [ info.email1, info.email2 ]
        self.emails = [ email for email in self.emails if email.strip() ]
        
        # notes
        self.notes = info.notes.replace('\r','').strip() or None
        

    def pprint(self):
        none = ""
        print "Name:    %s" % (self.name or none)
        print "Group:   %s" % (self.category or none)
        print "Company: %s" % (self.company or none)
        print "Title:   %s" % (self.title or none)
        
        print "Phones: ",
        if len(self.phones) == 0:
            print none
        m = max(([len(n[0]) for n in self.phones] or [0]))
        for i,num in enumerate(self.phones):
            if i>0:
                print "        ",
            print ("{text:%d} ({rel})"%(m)).format(text=num.text, rel=num.rel.split('#')[1].title())
        
        print "Email:  ", 
        if len(self.emails) == 0:
            print none
        m = max(([len(n[0]) for n in self.emails] or [0]))
        for i,email in enumerate(self.emails):
            if i>0:
                print "        ",
            print ("{text:%d} (Other)"%(m)).format(text=email)
        
        print "Notes:   %s" % (self.notes and "Skipped..." or none)

        for addr in self.addresses:
            print "%s Address:"%addr.rel.split('#')[1].title()
            print '\n'.join("   %s"%l for l in addr.text.split('\n'))
            
       

    def __getattr__(self, name):
        return None

def sync(info, entries, test=True):
    # setup client
    cli = ContactsService()
    cli.email = info['email']
    cli.password = info['password']
    cli.source = 'sas-contactsImport'
    cli.ProgrammaticLogin()
    
    # go through and find new groups
    groups = set("(Palm) "+c.category for c in entries)
    existing = [ e.title.text for e in cli.GetGroupsFeed().entry ]
    new_groups = groups.difference(existing)
    
    # create new groups
    for grp in new_groups:
        cli.CreateGroup(contacts.GroupEntry(title=atom.Title(text=grp)))
        
    # build group lookup dictionary
    feed = cli.GetGroupsFeed()
    groups_href = {}
    for entry in feed.entry:
        groups_href[ str(entry.title.text) ] = str(entry.id.text)
    
    # loop through contacts
    for contact in entries:
        if(test):
            contact.pprint()
            print ""
            continue
    
        # create basic info
        info = {}
        if contact.name:
            info['title'] = atom.Title(text=contact.name)
        
        # company info
        if contact.company or contact.title:
            org = contacts.Organization()
            org.org_name = contacts.OrgName(text=contact.company)
            org.org_title = contacts.OrgTitle(text=contact.title)
            info['organization'] = org
            
        # emails
        if contact.emails:
            info['email'] = [ contacts.Email(address=s) for s in contact.emails ]
            
        # phones
        if contact.phones:
            info['phone_number'] = [ contacts.PhoneNumber(text=num.text, rel=num.rel) for num in contact.phones ]
            
        # addresses
        if contact.addresses:
            info['postal_address'] = [ contacts.PostalAddress(text=addr.text, rel=addr.rel) for addr in contact.addresses ]
        
        # notes
        if contact.notes:
            info['content'] = atom.Content(text=contact.notes)
            
        # groups
        if contact.category:
            info['group_membership_info'] = [ contacts.GroupMembershipInfo(href=groups_href["(Palm) "+contact.category]) ]

        # save contact
        try:
            entry = contacts.ContactEntry(**info)
            cli.CreateContact(entry)
        except Exception as ex:
            print "\nTrouble with contact:"
            contact.pprint()
            print "----------------------------"
            traceback.print_exc()
            sys.exit()
                

if __name__=='__main__':
    from optparse import OptionParser
    import getpass
    
    parser = OptionParser()
    parser.add_option("-t", "--test", dest="test", default=False, action="store_true", help="pretty print then exit")
    parser.add_option("-f", "--file", dest="path", help="path to csv")
    
    opts,args = parser.parse_args()
   

    email = raw_input("Email: ")
    pwd = getpass.getpass("Pass:  ")

    login = { 'email': email, 'password': pwd }
    entries = [ Contact(ContactCSV(*r)) for r in csv.reader(open(opts.path, 'rb')) ]
    
    sync(login, entries, opts.test)

"""    

contact.group_membership_info.append(gdata.contacts.GroupMembershipInfo(href=groups["My Test Group"]))
gdataClient.UpdateContact(contact["entry"].GetEditLink().href, contact["entry"])

"""

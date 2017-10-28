PACKAGES := spacefortress spacefortress.game spacefortress.gym
PACKAGEDIRS := $(addprefix python/,$(PACKAGES))
PACKAGESCLEAN := $(addsuffix clean,$(PACKAGEDIRS))
PACKAGESIN := $(addsuffix install,$(PACKAGEDIRS))
PACKAGESUN := $(addsuffix uninstall,$(PACKAGEDIRS))

.PHONY = $(PACKAGE) $(PACKAGESCLEAN) clean

all: $(PACKAGES)

$(PACKAGES):
	$(MAKE) -C python/$@

clean: $(PACKAGESCLEAN)

%clean: %
	$(MAKE) -C $< clean

install: $(PACKAGESIN)

%install: %
	$(MAKE) -C $< install

uninstall: $(PACKAGESUN)

%uninstall: %
	$(MAKE) -C $< uninstall

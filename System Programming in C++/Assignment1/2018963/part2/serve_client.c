pthread_rwlock_t lock = PTHREAD_RWLOCK_INITIALIZER;

void* ServeClient(char* client) {
	char str[100];

	FILE* fp = fopen(client, "r+");
	if (fp == NULL)
	{
		printf("Failed to open.\n");
		exit(0);
	}
	else {
		while(1)
		{
			pthread_rwlock_wrlock(&lock);
			if (fgets(str, 100, fp) == NULL)
			{
				pthread_rwlock_unlock(&lock);
				break;
			}

			char* t;
			char* value;
			t = strtok(str, " ");

			if(strcmp(t, "insertNode") == 0)
			{ 
				value = strtok(NULL, " ");
				int parameter = atoi(value);
				root = insertNode(root, parameter);
				printf("[%s]insertNode <%u>\n", client, parameter);
			}

			else if(strcmp(t, "deleteNode") == 0)
			{
				value = strtok(NULL, " ");
				int parameter = atoi(value);
				root = deleteNode(root, parameter);
				printf("[%s]deleteNode <%u>\n", client, parameter);
			}
			
			else if(strcmp(t, "countNodes\n") == 0)
			{
				pthread_rwlock_unlock(&lock);
				pthread_rwlock_rdlock(&lock);
				int a = countNodes(root);
				printf("[%s]countNodes = <%d>\n", client, a);
				
			}
			
			else if (strcmp(t, "sumSubtree\n") == 0)
			{
				pthread_rwlock_unlock(&lock);
				pthread_rwlock_rdlock(&lock);
				int b = sumSubtree(root);
				printf("[%s]sumSubtree = <%d>\n", client, b);
			}
			pthread_rwlock_unlock(&lock);
		
		}
	}
	fclose(fp);
	return NULL;
}

